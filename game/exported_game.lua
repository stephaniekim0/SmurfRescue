do
local _ENV = _ENV
package.preload[ "enum" ] = function( ... ) local arg = _G.arg;
--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==
-- MIT License
--
-- Copyright (c) 2019 Skaruts (https://github.com/Skaruts/Lua-Enum)
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==--==
local fmt          = string.format
local remove       = table.remove
local floor         = math.floor
local ceil         = math.ceil
local abs          = math.abs
local type         = type
local next         = next
local select       = select
local tonumber     = tonumber
local setmetatable = setmetatable


local function _iterator(t, i)
	i = i+1
	local val = t[i]
	if i > #t then return end
	return i, val
end

local _make_globals = false

local Enum = {}
local MT = {
	__type = "enum", -- personal convention
	__index = function(t, k)
		return t._fields[k]
		or Enum[k]
		or t._iterable_values[k]
		or error(fmt("field %s does not exist in enum", k), 2)
	end,
	__newindex = function(t, k, v) error("cannot assign to an enum (enums are immutable)", 2) end,
	__tostring = function(t)
		local str = "enum: "
		for i=1, #t._ordered_fields do
			local k = t._ordered_fields[i]
			local v = t._fields[k]
			str = str .. fmt("%s = %d", k, v)
			if i < #t._ordered_fields then str = str .. ", " end
		end
		return str .. ""
	end,
	-- for lua 5.2+
	__ipairs = function(t) return _iterator, t._iterable_values, 0 end,
	__pairs = function(t) return next, t._fields, nil end,
}

-- for lua 5.1
function Enum:ipairs() return _iterator, self._iterable_values, 0 end
function Enum:pairs() return next, self._fields, nil end

-- for pretty printing - assembles the enum neatly over several lines and indented
function Enum:pstr()
	local str = "enum {\n"
	for i=1, #self._ordered_fields do
		local k = self._ordered_fields[i]
		local v = self._fields[k]
		str = str.. fmt(fmt("    %%-%ds%%d\n", self._longest_field+4), k, v)
	end
	return str .. "}"
end

function Enum.make_globals(enable)
	_make_globals = enable
end

local function _new_from_table(...)
	local t = {
		count = {},
		_fields = {},
		_iterable_values = {},
		_ordered_fields = {},
		_longest_field = 0,  -- for pretty printing
	}

	local exp = false    -- exponential stepping
	local step = 1       -- incremental step
	local elems = type(...) == "table" and ... or {...}

	-- check format
	local str = elems[1]:match("^[-+*%d]+")
	if str then
		remove(elems, 1)

		if tonumber(str) then
			step = tonumber(str)
		else
			if #str == 1 then
				if     str == '-' then step = -1
				elseif str == '+' then step = 1
				elseif str == '*' then
					step, exp = 2, true
				else
					error(fmt("invalid format '%s'", str))
				end
			else
				if str:sub(1, 1) ~= '*' then error(fmt("invalid format '%s'", str)) end
				step, exp = 2, true
				local inc = tonumber(str:match('%-?%d$'))
				if not inc and str:sub(2, 2) == '-' then inc = -2 end
				step = (inc and inc ~= 0) and inc or step
			end
		end
	end

	-- assemble the enum
	t.count = #elems
	local val = 0

	for i=1, #elems do
		local words = {}    -- try splitting the current entry into parts, if possible
		for word in elems[i]:gmatch("[%w_-]+") do
			words[#words+1] = word
		end

		-- check for duplicates
		local k = words[1]
		if t._fields[k] then error(fmt("duplicate field '%s' in enum", k), 2) end

		-- keep track of longest for pretty printing
		if #k > t._longest_field then t._longest_field = #k end

		-- if a second element exists then current entry contains a custom value
		if words[2] then val = tonumber(words[2]) end
		if not val then error(fmt("invalid value '%s' for enum field", words[2]), 2) end

		-- store the entries and respective values
		t._fields[k] = val
		t._ordered_fields[i] = k    -- useful for printing
		t._iterable_values[i] = val -- useful for iterators

		if _make_globals then
			_G[k] = val   -- not recommended
		end


		-- increase 'val' by increments or exponential growth
		if not exp then
			val = val + step
		else
			if val ~= 0 then
				if val > 0 and step < 0 then
					val = floor(val / abs(step))
				elseif val < 0 and step > 0 then
					-- val = val * step
					val = ceil(val / abs(step))
				else
					val = val * abs(step)
				end
			else
				val = step > 0 and 1 or -1
			end
		end
	end

	return setmetatable(t, MT)
end


local function _new_from_string(...)
	-- check if it's more than one string
	if select("#",...) > 1 then return _new_from_table(...) end

	-- remove comments
	local s = (...):gsub("%-%-[^\n]+", "")

	-- remove whitespace and ',' or '=', join custom values to their fields
	-- and put everything in a table
	local t = {}
	for word in s:gmatch('([^,\r\n\t =]+)') do
		if not tonumber(word) or #t == 0 then  -- if NAN or is format string
	    	t[#t+1] = word
	    else
			t[#t] = t[#t] .. " " .. tonumber(word)
		end
	end
	return _new_from_table(t)
end


local _constructors = {
	string = _new_from_string,
	table = _new_from_table,
}

local function _new(...)
	if not _constructors[type(...)] then error("invalid parameters for enum: must be a string, table or string varargs", 2) end
	return _constructors[type(...)](...)
end


return setmetatable( Enum, { __call = function(_, ...) return _new(...) end } )
end
end

do
local _ENV = _ENV
package.preload[ "middleclass" ] = function( ... ) local arg = _G.arg;
local middleclass = {
  _VERSION     = 'middleclass v4.1.1',
  _DESCRIPTION = 'Object Orientation for Lua',
  _URL         = 'https://github.com/kikito/middleclass',
  _LICENSE     = [[
    MIT LICENSE

    Copyright (c) 2011 Enrique García Cota

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  ]]
}

local function _createIndexWrapper(aClass, f)
  if f == nil then
    return aClass.__instanceDict
  elseif type(f) == "function" then
    return function(self, name)
      local value = aClass.__instanceDict[name]

      if value ~= nil then
        return value
      else
        return (f(self, name))
      end
    end
  else -- if  type(f) == "table" then
    return function(self, name)
      local value = aClass.__instanceDict[name]

      if value ~= nil then
        return value
      else
        return f[name]
      end
    end
  end
end

local function _propagateInstanceMethod(aClass, name, f)
  f = name == "__index" and _createIndexWrapper(aClass, f) or f
  aClass.__instanceDict[name] = f

  for subclass in pairs(aClass.subclasses) do
    if rawget(subclass.__declaredMethods, name) == nil then
      _propagateInstanceMethod(subclass, name, f)
    end
  end
end

local function _declareInstanceMethod(aClass, name, f)
  aClass.__declaredMethods[name] = f

  if f == nil and aClass.super then
    f = aClass.super.__instanceDict[name]
  end

  _propagateInstanceMethod(aClass, name, f)
end

local function _tostring(self) return "class " .. self.name end
local function _call(self, ...) return self:new(...) end

local function _createClass(name, super)
  local dict = {}
  dict.__index = dict

  local aClass = { name = name, super = super, static = {},
                   __instanceDict = dict, __declaredMethods = {},
                   subclasses = setmetatable({}, {__mode='k'})  }

  if super then
    setmetatable(aClass.static, {
      __index = function(_,k)
        local result = rawget(dict,k)
        if result == nil then
          return super.static[k]
        end
        return result
      end
    })
  else
    setmetatable(aClass.static, { __index = function(_,k) return rawget(dict,k) end })
  end

  setmetatable(aClass, { __index = aClass.static, __tostring = _tostring,
                         __call = _call, __newindex = _declareInstanceMethod })

  return aClass
end

local function _includeMixin(aClass, mixin)
  assert(type(mixin) == 'table', "mixin must be a table")

  for name,method in pairs(mixin) do
    if name ~= "included" and name ~= "static" then aClass[name] = method end
  end

  for name,method in pairs(mixin.static or {}) do
    aClass.static[name] = method
  end

  if type(mixin.included)=="function" then mixin:included(aClass) end
  return aClass
end

local DefaultMixin = {
  __tostring   = function(self) return "instance of " .. tostring(self.class) end,

  initialize   = function(self, ...) end,

  isInstanceOf = function(self, aClass)
    return type(aClass) == 'table'
       and type(self) == 'table'
       and (self.class == aClass
            or type(self.class) == 'table'
            and type(self.class.isSubclassOf) == 'function'
            and self.class:isSubclassOf(aClass))
  end,

  static = {
    allocate = function(self)
      assert(type(self) == 'table', "Make sure that you are using 'Class:allocate' instead of 'Class.allocate'")
      return setmetatable({ class = self }, self.__instanceDict)
    end,

    new = function(self, ...)
      assert(type(self) == 'table', "Make sure that you are using 'Class:new' instead of 'Class.new'")
      local instance = self:allocate()
      instance:initialize(...)
      return instance
    end,

    subclass = function(self, name)
      assert(type(self) == 'table', "Make sure that you are using 'Class:subclass' instead of 'Class.subclass'")
      assert(type(name) == "string", "You must provide a name(string) for your class")

      local subclass = _createClass(name, self)

      for methodName, f in pairs(self.__instanceDict) do
        if not (methodName == "__index" and type(f) == "table") then
          _propagateInstanceMethod(subclass, methodName, f)
        end
      end
      subclass.initialize = function(instance, ...) return self.initialize(instance, ...) end

      self.subclasses[subclass] = true
      self:subclassed(subclass)

      return subclass
    end,

    subclassed = function(self, other) end,

    isSubclassOf = function(self, other)
      return type(other)      == 'table' and
             type(self.super) == 'table' and
             ( self.super == other or self.super:isSubclassOf(other) )
    end,

    include = function(self, ...)
      assert(type(self) == 'table', "Make sure you that you are using 'Class:include' instead of 'Class.include'")
      for _,mixin in ipairs({...}) do _includeMixin(self, mixin) end
      return self
    end
  }
}

function middleclass.class(name, super)
  assert(type(name) == 'string', "A name (string) is needed for the new class")
  return super and super:subclass(name) or _includeMixin(_createClass(name), DefaultMixin)
end

setmetatable(middleclass, { __call = function(_, ...) return middleclass.class(...) end })

return middleclass
end
end

do
local _ENV = _ENV
package.preload[ "std.strict" ] = function( ... ) local arg = _G.arg;
--[[
 Strict variable declarations for Lua 5.1, 5.2, 5.3 & 5.4.
 Copyright (C) 2006-2023 std.strict authors
]]
--[[--
 Diagnose uses of undeclared variables.

 All variables(including functions!) must be "declared" through a regular
 assignment(even assigning `nil` will do) in a strict scope before being
 used anywhere or assigned to inside a nested scope.

 Use the callable returned by this module to interpose a strictness check
 proxy table to the given environment.   The callable runs `setfenv`
 appropriately in Lua 5.1 interpreters to ensure the semantic equivalence.

 @module std.strict
]]


local setfenv = rawget(_G, 'setfenv') or function() end
local debug_getinfo = debug.getinfo


-- Return callable objects.
-- @function callable
-- @param x an object or primitive
-- @return *x* if *x* can be called, otherwise `nil`
-- @usage
--   (callable(functable) or function()end)(args, ...)
local function callable(x)
   -- Careful here!
   -- Most versions of Lua don't recurse functables, so make sure you
   -- always put a real function in __call metamethods.  Consequently,
   -- no reason to recurse here.
   -- func=function() print 'called' end
   -- func() --> 'called'
   -- functable=setmetatable({}, {__call=func})
   -- functable() --> 'called'
   -- nested=setmetatable({}, {__call=function(self, ...) return functable(...)end})
   -- nested() -> 'called'
   -- notnested=setmetatable({}, {__call=functable})
   -- notnested()
   -- --> stdin:1: attempt to call global 'nested' (a table value)
   -- --> stack traceback:
   -- -->	stdin:1: in main chunk
   -- -->		[C]: in ?
   if type(x) == 'function' or (getmetatable(x) or {}).__call then
      return x
   end
end


-- Return named metamethod, if callable, otherwise `nil`.
-- @param x item to act on
-- @string n name of metamethod to look up
-- @treturn function|nil metamethod function, if callable, otherwise `nil`
local function getmetamethod(x, n)
   return callable((getmetatable(x) or {})[n])
end


-- Length of a string or table object without using any metamethod.
-- @function rawlen
-- @tparam string|table x object to act on
-- @treturn int raw length of *x*
-- @usage
--    --> 0
--    rawlen(setmetatable({}, {__len=function() return 42}))
local function rawlen(x)
   -- Lua 5.1 does not implement rawlen, and while # operator ignores
   -- __len metamethod, `nil` in sequence is handled inconsistently.
   if type(x) ~= 'table' then
      return #x
   end

   local n = #x
   for i = 1, n do
      if x[i] == nil then
         return i -1
      end
   end
   return n
end


-- Deterministic, functional version of core Lua `#` operator.
--
-- Respects `__len` metamethod (like Lua 5.2+).   Otherwise, always return
-- one less than the lowest integer index with a `nil` value in *x*, where
-- the `#` operator implementation might return the size of the array part
-- of a table.
-- @function len
-- @param x item to act on
-- @treturn int the length of *x*
-- @usage
--    x = {1, 2, 3, nil, 5}
--    --> 5 3
--    print(#x, len(x))
local function len(x)
   return (getmetamethod(x, '__len') or rawlen)(x)
end


-- Like Lua `pairs` iterator, but respect `__pairs` even in Lua 5.1.
-- @function pairs
-- @tparam table t table to act on
-- @treturn function iterator function
-- @treturn table *t*, the table being iterated over
-- @return the previous iteration key
-- @usage
--    for k, v in pairs {'a', b='c', foo=42} do process(k, v) end
local pairs = (function(f)
   if not f(setmetatable({},{__pairs=function() return false end})) then
      return f
   end

   return function(t)
      return(getmetamethod(t, '__pairs') or f)(t)
   end
end)(pairs)


-- What kind of variable declaration is this?
-- @treturn string 'C', 'Lua' or 'main'
local function what()
   local d = debug_getinfo(3, 'S')
   return d and d.what or 'C'
end


return setmetatable({
   --- Module table.
   -- @table strict
   -- @string version release version identifier


   --- Require variable declarations before use in scope *env*.
   --
   -- Normally the module @{strict:__call} metamethod is all you need,
   -- but you can use this method for more complex situations.
   -- @function strict
   -- @tparam table env lexical environment table
   -- @treturn table *env* proxy table with metamethods to enforce strict
   --    declarations
   -- @usage
   --   local _ENV = setmetatable({}, {__index = _G})
   --   if require 'std._debug'.strict then
   --      _ENV = require 'std.strict'.strict(_ENV)
   --   end
   --   -- ...and for Lua 5.1 compatibility, without triggering undeclared
   --   -- variable error:
   --   if rawget(_G, 'setfenv') ~= nil then
   --      setfenv(1, _ENV)
   --   end
   strict = function(env)
      -- The set of declared variables in this scope.
      local declared = {}

      --- Environment Metamethods
      -- @section environmentmetamethods

      return setmetatable({}, {
         --- Detect dereference of undeclared variable.
         -- @function env:__index
         -- @string n name of the variable being dereferenced
         __index = function(_, n)
            local v = env[n]
            if v ~= nil then
               declared[n] = true
            elseif not declared[n] and what() ~= 'C' then
               error("variable '" .. n .. "' is not declared", 2)
            end
            return v
         end,

         --- Proxy `len` calls.
         -- @function env:__len
         -- @tparam table t strict table
         __len = function() return len(env) end,

         --- Detect assignment to undeclared variable.
         -- @function env:__newindex
         -- @string n name of the variable being declared
         -- @param v initial value of the variable
         __newindex = function(_, n, v)
            local x = env[n]
            if x == nil and not declared[n] then
               local w = what()
               if w ~= 'main' and w ~= 'C' then
                  error("assignment to undeclared variable '" .. n .. "'", 2)
               end
            end
            declared[n] = true
            env[n] = v
         end,

         --- Proxy `pairs` calls.
         -- @function env:__pairs
         -- @tparam table t strict table
         __pairs = function()
            return pairs(env)
         end,
      })
   end,
}, {
   --- Module Metamethods
   -- @section modulemetamethods

   --- Enforce strict variable declarations in *env*.
   -- @function strict:__call
   -- @tparam table env lexical environment table
   -- @tparam[opt=1] int level stack level for `setfenv`, 1 means
   --    set caller's environment
   -- @treturn table *env* which must be assigned to `_ENV`
   -- @usage
   --   local _ENV = require 'std.strict'(_G)
   __call = function(self, env, level)
      env = self.strict(env)
      setfenv(1 + (level or 1), env)
      return env
   end,

   --- Lazy loading of strict submodules.
   -- Don't load everything on initial startup, wait until first attempt
   -- to access a submodule, and then load it on demand.
   -- @function __index
   -- @string name submodule name
   -- @treturn table|nil the submodule that was loaded to satisfy the missing
   --    `name`, otherwise `nil` if nothing was found
   -- @usage
   --   local strict = require 'std.strict'
   --   local version = strict.version
   __index = function(self, name)
      local ok, t = pcall(require, 'std.strict.' .. name)
      if ok then
         rawset(self, name, t)
         return t
      end
   end,
})
end
end

do
local _ENV = _ENV
package.preload[ "timer" ] = function( ... ) local arg = _G.arg;
--[[
Copyright (c) 2010-2013 Matthias Richter

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

Except as contained in this notice, the name(s) of the above copyright holders
shall not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
]]--

local Timer = {}
Timer.__index = Timer

local function _nothing_() end

local function updateTimerHandle(handle, dt)
		-- handle: {
		--   time = <number>,
		--   after = <function>,
		--   during = <function>,
		--   limit = <number>,
		--   count = <number>,
		-- }
		handle.time = handle.time + dt
		handle.during(dt, math.max(handle.limit - handle.time, 0))

		while handle.time >= handle.limit and handle.count > 0 do
			if handle.after(handle.after) == false then
				handle.count = 0
				break
			end
			handle.time = handle.time - handle.limit
			handle.count = handle.count - 1
		end
end

function Timer:update(dt)
	-- timers may create new timers, which leads to undefined behavior
	-- in pairs() - so we need to put them in a different table first
	local to_update = {}
	for handle in pairs(self.functions) do
		to_update[handle] = handle
	end

	for handle in pairs(to_update) do
		if self.functions[handle] then
			updateTimerHandle(handle, dt)
			if handle.count == 0 then
				self.functions[handle] = nil
			end
		end
	end
end

function Timer:during(delay, during, after)
	local handle = { time = 0, during = during, after = after or _nothing_, limit = delay, count = 1 }
	self.functions[handle] = true
	return handle
end

function Timer:after(delay, func)
	return self:during(delay, _nothing_, func)
end

function Timer:every(delay, after, count)
	local count = count or math.huge -- exploit below: math.huge - 1 = math.huge
	local handle = { time = 0, during = _nothing_, after = after, limit = delay, count = count }
	self.functions[handle] = true
	return handle
end

function Timer:cancel(handle)
	self.functions[handle] = nil
end

function Timer:clear()
	self.functions = {}
end

function Timer:script(f)
	local co = coroutine.wrap(f)
	co(function(t)
		self:after(t, co)
		coroutine.yield()
	end)
end

Timer.tween = setmetatable({
	-- helper functions
	out = function(f) -- 'rotates' a function
		return function(s, ...) return 1 - f(1-s, ...) end
	end,
	chain = function(f1, f2) -- concatenates two functions
		return function(s, ...) return (s < .5 and f1(2*s, ...) or 1 + f2(2*s-1, ...)) * .5 end
	end,

	-- useful tweening functions
	linear = function(s) return s end,
	quad   = function(s) return s*s end,
	cubic  = function(s) return s*s*s end,
	quart  = function(s) return s*s*s*s end,
	quint  = function(s) return s*s*s*s*s end,
	sine   = function(s) return 1-math.cos(s*math.pi/2) end,
	expo   = function(s) return 2^(10*(s-1)) end,
	circ   = function(s) return 1 - math.sqrt(1-s*s) end,

	back = function(s,bounciness)
		bounciness = bounciness or 1.70158
		return s*s*((bounciness+1)*s - bounciness)
	end,

	bounce = function(s) -- magic numbers ahead
		local a,b = 7.5625, 1/2.75
		return math.min(a*s^2, a*(s-1.5*b)^2 + .75, a*(s-2.25*b)^2 + .9375, a*(s-2.625*b)^2 + .984375)
	end,

	elastic = function(s, amp, period)
		amp, period = amp and math.max(1, amp) or 1, period or .3
		return (-amp * math.sin(2*math.pi/period * (s-1) - math.asin(1/amp))) * 2^(10*(s-1))
	end,
}, {

-- register new tween
__call = function(tween, self, len, subject, target, method, after, ...)
	-- recursively collects fields that are defined in both subject and target into a flat list
	local function tween_collect_payload(subject, target, out)
		for k,v in pairs(target) do
			local ref = subject[k]
			assert(type(v) == type(ref), 'Type mismatch in field "'..k..'".')
			if type(v) == 'table' then
				tween_collect_payload(ref, v, out)
			else
				local ok, delta = pcall(function() return (v-ref)*1 end)
				assert(ok, 'Field "'..k..'" does not support arithmetic operations')
				out[#out+1] = {subject, k, delta}
			end
		end
		return out
	end

	method = tween[method or 'linear'] -- see __index
	local payload, t, args = tween_collect_payload(subject, target, {}), 0, {...}

	local last_s = 0
	return self:during(len, function(dt)
		t = t + dt
		local s = method(math.min(1, t/len), unpack(args))
		local ds = s - last_s
		last_s = s
		for _, info in ipairs(payload) do
			local ref, key, delta = unpack(info)
			ref[key] = ref[key] + delta * ds
		end
	end, after)
end,

-- fetches function and generated compositions for method `key`
__index = function(tweens, key)
	if type(key) == 'function' then return key end

	assert(type(key) == 'string', 'Method must be function or string.')
	if rawget(tweens, key) then return rawget(tweens, key) end

	local function construct(pattern, f)
		local method = rawget(tweens, key:match(pattern))
		if method then return f(method) end
		return nil
	end

	local out, chain = rawget(tweens,'out'), rawget(tweens,'chain')
	return construct('^in%-([^-]+)$', function(...) return ... end)
	       or construct('^out%-([^-]+)$', out)
	       or construct('^in%-out%-([^-]+)$', function(f) return chain(f, out(f)) end)
	       or construct('^out%-in%-([^-]+)$', function(f) return chain(out(f), f) end)
	       or error('Unknown interpolation method: ' .. key)
end})

-- Timer instancing
function Timer.new()
	return setmetatable({functions = {}, tween = Timer.tween}, Timer)
end

-- default instance
local default = Timer.new()

-- module forwards calls to default instance
local module = {}
for k in pairs(Timer) do
	if k ~= "__index" then
		module[k] = function(...) return default[k](default, ...) end
	end
end
module.tween = setmetatable({}, {
	__index = Timer.tween,
	__newindex = function(k,v) Timer.tween[k] = v end,
	__call = function(t, ...) return default:tween(...) end,
})

return setmetatable(module, {__call = Timer.new})
end
end

-- title:   Smurf - Rescue in Gargamel's Castle (REMAKE)
-- author:  Sumin Byun, Stephanie Kim
-- desc:    TIC-80 remake of Atari 2600's Smurf: Rescue in Gargamel's Castle (1982)
-- site:    https://github.swarthmore.edu/pages/CS91S-F24/remake-sbyun1-skim9/game/
-- license: MIT License
-- version: 0.1
-- script:  lua

local _ENV = require 'std.strict' (_G)
local class = require 'middleclass'
local Smurf = class('Smurf')
local Vector2 = class('Vector2')
local Timer = require 'timer'
local Enum = require('enum')

-- variables of the smurf object
function Smurf:initialize(x, y, vx, vy)
   self.v = Vector2:new(x, y) --x and y position
   self.velocity = 0
   self.faceflag = 0 --facing right: 0, facing left: 1
   self.vx = vx 
   self.vy = vy 
   self.alive = 1 --alive: 1, dead: 0
   self.lifespan = 100  --for countdown bar
   self.score = 0
   self.lives = 4  --user starts with 4 lives
   self.rescued = 0 --reached smurfette or not
   self.tile_type = mget(math.floor((self.v.x)/8), math.floor((self.v.y)/8)) --what tile smurf's feet are on
   self.gravity = 0.2 
   self.ground_level = -1 
   self.add_x_byScene = 0 --x adjustments depending on map
   self.add_y_byScene = 0 --y adjustments depending on map
   self.pressed = -1 --time when up arrow pressed
   self.collided = 1 --time when smurf collides
   self.collision_type = 1 --collision type (real or fake)
end

--x and y positions of the smurf
function Vector2:initialize(x, y)
   self.x = x
   self.y = y
end

--left and right movement of the smurf
function Vector2:add(o)
   self.x = self.x + o.x
   self.y = self.y + o.y
end

-- time
t = time()
--starting location of the smurf
local smf = Smurf:new(65, 90, 0, 0)
--possible jump states of the smurf
local Button = Enum("unpressed", "single", "double")
--current jump states of the smurf
local press_type = 0
--tile numbers for obstacles
--fence (24,25,26,41,42,243), water (240,247,248), green steps (241), 
--purple steps (242), spider (245), buildings (242), falling (244) 
local collision_tiles = {24,25,26,41,42,243,240,247,248, 241,245,242,244}
local s = 0
--left movement
local left = Vector2:new(-1, 0)
--right movement
local right = Vector2:new(1, 0)
--current map smurf is on
local current_map = 0
--instruction screnn
local startGame = 0

--SCORE FLAGS
local points = {
   fence_points = 0,
   water_points = 0,
   stair1_points = 0,
   stair2_points = 0,
   purple1_points = 0,
   purple2_points = 0,
   building1_points = 0,
   building2_points = 0,
   girl_points = 0
}

--MAP FLAGS
local entered = {
   house_map = 0,
   fence_map = 0,
   water_map = 0,
   stairs_map = 0,
   purple_map = 0,
   smurfette_map = 0
}

--draws and establishes ground levels for each map
function Smurf:drawMap()
  --opening scene
   if self.v.x <= 230 then 
      current_map = 1
      map(0,0,30,17,0,0)
      self.ground_level = 90
   --fence scene
   elseif (self.v.x > 230 and self.v.x <= 460) then
      current_map = 2
      map(30,0,30,17,0,0) 
      self.ground_level = 90
   --water scene
   elseif (self.v.x > 460 and self.v.x <= 690) then
      current_map = 3
      map(60,0,30,17,0,0)
      self.ground_level = 90
   --green steps
   elseif (self.v.x > 690 and self.v.x <= 920) then
      current_map = 4
      map(90,0,30,17,0,0) 
      if (self.v.x > 690 and self.v.x <= 760) then
         self.ground_level = 93
      elseif (self.v.x > 760 and self.v.x <= 820) then 
         self.ground_level = 77 
      elseif (self.v.x > 820 and self.v.x <= 920) then
         self.ground_level = 61  
      end
   --purple steps
   elseif (self.v.x > 920 and self.v.x < 1150) then
      current_map = 5
      map(120,0,30,17,0,0)
      drawWeb() --draw spiderweb
      if (self.v.x > 920 and self.v.x < 948) then
         self.ground_level = 69
      elseif (self.v.x > 948 and self.v.x < 958) then
         self.ground_level = 77
      elseif (self.v.x > 958 and self.v.x < 968) then
         self.ground_level = 85
      elseif (self.v.x > 968 and self.v.x < 978) then
         self.ground_level = 93
      elseif (self.v.x > 978 and self.v.x < 1084) then
         self.ground_level = 101
      elseif (self.v.x > 1084 and self.v.x < 1094) then
         self.ground_level = 93
      elseif (self.v.x > 1094 and self.v.x < 1104) then
         self.ground_level = 85
      elseif (self.v.x > 1104 and self.v.x < 1114) then
         self.ground_level = 77
      elseif (self.v.x > 1114 and self.v.x < 1150) then
         self.ground_level = 69
      end
   --smurfette scene
   elseif (self.v.x > 1150) then
      current_map = 6 
      map(150,0,30,17,0,0)
      if self.v.x < 1336 then
         spr(135,210,15,0,1,0,0,2,4) --draw smurfette before rescue
      end
      self.ground_level = 85
      if (self.v.x > 1180 and self.v.x < 1219) then
         self.ground_level = 61
      elseif (self.v.x >= 1219 and self.v.x < 1229) then
         self.ground_level = 85
      elseif (self.v.x >= 1229 and self.v.x < 1252) then
         self.ground_level = 45
      elseif (self.v.x >= 1252 and self.v.x < 1270) then
         self.ground_level  = 21
      elseif (self.v.x >= 1270 and self.v.x < 1298) then
         self.ground_level = 45
      elseif (self.v.x > 1312 and self.v.x < 1365) then
         self.ground_level = 13
      end
   end
end

--draws lives count on the screen with smurf heads
function Smurf:drawLives()
   for i = 1,self.lives,1 do
      spr(230,93+(10*i),8,0,1,0,0,1,2)
   end
end

--draws spiderweb in purple scene
function drawWeb()
   line(115,15,115,55,12)
   line(95,35,135,35,12)

   line(135,35,115,15,12)
   line(136,35,116,15,12)

   line(115,55,135,35,12)
   line(116,55,136,35,12)

   line(95,35,115,55,12)
   line(96,35,116,55,12)

   line(115,15,95,35,12)
   line(116,15,96,35,12)


   line(105,25,125,45,12)
   line(106,25,126,45,12)

   line(105,45,125,25,12)
   line(106,45,126,25,12)

   line(105,35,115,25,12)
   line(106,35,116,25,12)

   line(115,25,125,35,12)
   line(116,25,126,35,12)

   line(125,35,115,45,12)
   line(126,35,116,45,12)

   line(115,45,105,35,12)
   line(116,45,106,35,12)
end

--updates score when smurf passes obstacles
function Smurf:updateScore()
   if self.v.x > 300 and points.fence_points==0 then
      points.fence_points = 400
      self.score = self.score + points.fence_points
   end
   if self.v.x > 572 and points.water_points==0 then
      points.water_points = 400
      self.score = self.score + points.water_points
   end
   if self.v.x > 767 and points.stair1_points==0 then
      points.stair1_points = 400
      self.score = self.score + points.stair1_points
   end
   if self.v.x > 823 and points.stair2_points==0 then
      points.stair2_points = 400
      self.score = self.score + points.stair2_points
   end
   if self.v.x > 985 and points.purple1_points==0 then
      points.purple1_points = 400
      self.score = self.score + points.purple1_points
   end
   if self.v.x > 1116 and points.purple2_points==0 then
      points.purple2_points = 400
      self.score = self.score + points.purple2_points
   end
   if self.v.x > 1188 and points.building1_points==0 then
      points.building1_points = 400
      self.score = self.score + points.building1_points
   end
   if self.v.x > 1250 and points.building2_points==0 then
      points.building2_points = 400
      self.score = self.score + points.building2_points
   end
   if self.v.x > 1336 and points.girl_points==0 then
      points.girl_points = 1000
      self.score = self.score + points.girl_points
   end
   print("SCORE: "..self.score,95,3,12) --display on screen
end

--tracks time spent in each map
function Smurf:countDown()
   if self.v.x > 1 and self.v.x <= 230 and entered.house_map==0 then
      entered.house_map = t
   end
   if self.v.x > 230 and self.v.x <= 460 and entered.fence_map==0 then
      entered.fence_map = t
      self.lifespan = 100
   elseif self.v.x > 460 and self.v.x <= 690 and entered.water_map==0 then
      entered.water_map = t
      self.lifespan = 100
   elseif self.v.x > 690 and self.v.x <= 920 and entered.stairs_map==0 then
      entered.stairs_map = t
      self.lifespan = 100
   elseif self.v.x > 920 and self.v.x <= 1150 and entered.purple_map==0 then
      entered.purple_map = t
      self.lifespan = 100
   elseif self.v.x > 1150 and entered.smurfette_map==0 then
      entered.smurfette_map = t
      self.lifespan = 100
   end
end

--determines jumping state and does appropriate jumping action
function Smurf:physics()
   --jumping state (unpressed, single, double)
   if self.pressed > 0 and btnp(0) and (t-self.pressed) < 1000 and self.vy==0 and self.alive == 1 then
      press_type = Button.double
      self.pressed = t
   elseif btnp(0) and self.vy==0 and self.alive == 1 then
      self.pressed = t
      press_type = Button.single
   else 
      press_type = Button.unpressed
   end

   -- JUMP PHYSICS
   if press_type==Button.double then
      --jump forward
      if self.faceflag == 0 then
         self.vx = 1.75
         self.vy = -3.5
      elseif self.faceflag == 1 and self.v.x >=128 then 
         self.vx = -1.75
         self.vy = -3.5
      --prevent jumping left at start of game (towards house)
      elseif self.faceflag == 1 and self.v.x <128 then 
         self.vy = -3
      end
   elseif press_type == Button.single then
      --jump up
      self.vy = -3 
   elseif self.v.y >= self.ground_level then
      --stop on ground
      self.vy=0
      self.v.y = self.ground_level 
      self.vx = 0
   else 
      --come back down
      self.vy = self.vy + self.gravity
   end
   --update x and y based on velocity
   self.v.x = self.v.x+self.vx
   self.v.y = self.v.y+self.vy

end

--smurf movements based on user input
function Smurf:input()
   --buttons: up=0, down=1, left=2, right=3
   if self.alive == 1 and self.rescued==0 then
      if btn(0) then 
         --display jumping smurf sprite
         spr(11,self.v.x%230,self.v.y,0,1,self.faceflag,0,2,4)
      elseif btn(1) then 
         --display squatting smurf sprite
         spr(189,self.v.x%230,self.v.y+5,0,1,self.faceflag,0,2,3)
      elseif btn(2) then 
         if self.v.x >=65 then
            self.v:add(left) 
            spr(71+s%60//20*3,self.v.x%230,self.v.y,0,1,1,0,2,4)
            self.faceflag=1
         else
            --display still smurf sprite if attempting to walk to house
            spr(71+s%60//20*3,65,90,0,1,1,0,2,4)
         end
      elseif btn(3) then 
         --display walking smurf sprite
         if self.v.x <=1340 then
            self.v:add(right) 
            spr(71+s%60//20*3,self.v.x%230,self.v.y,0,1,0,0,2,4)
            self.faceflag=0
         end
      else 
         --display still smurf sprite as default
         if self.v.x <= 1340 then
            spr(71,self.v.x%230,self.v.y,0,1,self.faceflag,0,2,4) 
         end
      end
   end
end

--finds tile that smurf's feet are on after adjustions based on map
function Smurf:collision_location()
   if current_map == 2 then --fence scene
      self.add_x_byScene = 25
      self.add_y_byScene = 22
   elseif current_map == 3 then --water scene
      self.add_x_byScene = 30
      self.add_y_byScene = 22
   elseif current_map == 4 then --green steps scene
      self.add_x_byScene = 45
      self.add_y_byScene = 22
   elseif current_map == 5 then --spider scene
      self.add_x_byScene = 55
      self.add_y_byScene = 19
   elseif current_map == 6 then --building scene
      self.add_x_byScene = 64
      self.add_y_byScene = 19
   end
   self.tile_type = mget(math.floor((self.v.x)/8), math.floor((self.v.y)/8))
   --gets tile that smurf's feet are on 
   if self.faceflag == 0 then
      self.tile_type = mget(math.floor((self.v.x+self.add_x_byScene)/8), math.floor((self.v.y+self.add_y_byScene)/8))
   elseif self.faceflag == 1 then
      self.tile_type = mget(math.floor((self.v.x+self.add_x_byScene-10)/8), math.floor((self.v.y+self.add_y_byScene)/8))
   end
end

--determines collision type and does appropriate action
function Smurf:collision()
   --real collision: -1, fake collision: 0
   t = time()
   local how_far = 0 --how far smurf is from start of map
   --if smurf is on collision tile
   if contains(collision_tiles, self.tile_type) then
      --if collision happened within 0.05s of previous collision
      if self.collided > 0 and (t-self.collided) < 50 then
         --fake collision
         self.collision_type = 0 
         self.collided = t
      else
         --real collision
         self.collided = t
         self.collision_type = -1
      end
   end

   if self.collision_type == -1 then
      --real collision
      self.alive = 0
      how_far = (self.v.x%230) - 5
      Timer.during(2000, function() spr(114,self.v.x%230,self.v.y+5,0,1,self.faceflag,0,2,3) end)
      Timer.after(2000, function() self.v.x = self.v.x-how_far end)
      Timer.after(2000, function() self.faceflag = 0 end) --reposition facing right side
      Timer.after(2000, function() self.lifespan=100 end)
      self:countdownBar()
      self.lives = self.lives - 1
      if self.lives > 0 then
         Timer.after(2000, function() self.alive=1 end)
      else
         --game over
         map(180,0,30,17,0,0)

      end
      self.collision_type = 1
   elseif self.collision_type == 0 then
      --fake collision    
      self.collision_type = 1
   end
end

--game over (smurfette rescued or all lives lost)
function Smurf:rescue()
   if self.v.x > 1340 then
      --smurfette rescued
      self.alive = 1
      self.rescued = 1
      spr(201,1345%230,15,0,1,0,0,4,4) --draws rescued smurf and smurfette
      spr(141,203,8,0,1,0,0,2,1) -- draws heart
      print("CONGRATULATIONS!",25,70,12,1,2) -- game over text
   end
   if self.lives<=0 then
      --makes smurf dead and displays game over screen
      smf.alive=0
      map(180,0,30,17,0,0)
      print("FINAL SCORE: "..self.score,35,40, 12, 0, 2)
      print("GAME OVER!", 35, 80, 8, 0, 3)
   end
end

--updates countdown bar for each map
function Smurf:countdownBar()
   if self.rescued==0 then
      --steadily decrease countdown bar to last 10s
      self.lifespan = self.lifespan-.165
      rect(70,11,self.lifespan, 11, 0)
      --if countdown bar done
      if math.floor(self.lifespan)==0 and self.lives >=1 then
         --reset countdown bar and decrease lives count
         self.lifespan=100
         self.lives=self.lives-1
      end
   else
      --stop countdown bar when smurfette rescued
      rect(70,11,self.lifespan, 11, 0)
   end
end

--draws moving spider in purple scene (5th map)
function Smurf:drawMovingSprite()   
   if (self.v.x > 920 and self.v.x < 1150) then 
      spr(195+s%90//30*2,110,112,0,1,0,0,2,2)
   end
end

--check if value in table (for collision tiles/tile type) 
function contains(tab, val)
   for index, value in ipairs(tab) do
      if value == val then
         return true
      end
   end
   return false
end

--show instructions before starting game
function start(startGame)
   map(0,17,30,17,0,0)
   print("Jump over obstacles to rescue Smurfette!", 8, 5, 12)
   print("GAMEPAD (arrow keys):", 68, 30, 12)
   print("up", 20, 105, 12)
   print("up twice", 50, 105, 12)
   print("left", 118, 105, 12)
   print("right", 165, 105, 12)
   print("down", 205, 105, 12)
   print("Press 'Z' to start", 75, 130, 12)
end

function TIC()
   cls(13)

   --if game hasn't started
   if startGame == 0 then
      start(startGame)
      --start game when 'Z' pressed
      if btn(4) then
         startGame = 1
      end
   end

   --after game has started
   if startGame == 1 then 
      smf:drawMap()
      smf:countdownBar()
      smf:drawLives()
      smf:updateScore()
      smf:countDown()

      local dt = time() - t
      Timer.update(dt)

      smf:physics()
      smf:collision_location()
      smf:collision()
      smf:input()
      smf:drawMovingSprite()
      smf:rescue() 
   end
   s=s+4
end

-- <TILES>
-- 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
-- 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
-- 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
-- 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
-- 005:5555555555555555555555555555555555555555555555555555555555555555
-- 006:6666666666666666666666666666666666666666666666666666666666666666
-- 007:7777777777777777777777777777777777777777777777777777777777777777
-- 008:3333333333333333333333333333333333333333333333333333333333333333
-- 009:1111111111111111111111111111111111111111111111111111111111111111
-- 010:2222222222222222222222222222222222222222222222222222222222222222
-- 011:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 012:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 014:0000000000000000000ccccc0ccccccc0ccc00cc0000cccc00cccccccccccccc
-- 015:000000000000000000000000cc000000cccc0000cccc0000cccccc00cccccc00
-- 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- 021:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 022:88888888777777779999999977777777dddddddd7777777788888888dddddddd
-- 023:dddddddd3333333333333333bbbbbbbb3333333333333333dddddddd33333333
-- 024:ccccffffcccccccfccccccccfffcccccffffffccffffffffffffffffccffffff
-- 025:ffffffffffffffffcccfffffccccccffccccccccffccccccfffffcccfffffffc
-- 026:66666666666666666666666666666666666666666666666666666666cc666666
-- 027:0000009900999999009999990099999000000999000009990000000900000999
-- 028:9009000090099990999999000999990090000000999990009900000099000000
-- 030:0000900909999009009999990099999000000009990999999990009999009999
-- 031:9900000099999900999999000999990099900000999000009000000099999900
-- 033:1111111111111111111111111111111111111111111111111111111111111111
-- 034:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 035:0000000c000000cc00000ccc0000cccc000ccccc00cccccc0ccccccccccccccc
-- 036:c0000000cc000000ccc00000cccc0000ccccc000cccccc00ccccccc0cccccccc
-- 038:88888888dddddddd99999999aaaaaaaabbbbbbbbddddddddbbbbbbbb33333333
-- 039:3333333344444444444444444444444433333333444444444444444422222222
-- 041:cccc6666cccccc6666cccccc6666cccc666666cc666666666666666666666666
-- 042:6666666666666666c6666666cccc6666cccccc666ccccccc6666cccc666666cc
-- 043:0099999900990999009909990099099900000ccc000000cc000000cc00000000
-- 044:99999900999000009999000099990000cccc0000ccccc000cccccc00ccccc000
-- 046:990099999999999999999999000099990000cccc0000cccc0000c00cccccc000
-- 047:99999900999009009009990099909900ccc00000ccc00000cccccc00000ccc00
-- 048:bbccccbbbbccccbbbbccccbbbbccccbbbbccccbbbbccccbbbbccccbbbbccccbb
-- 049:4444444444444444444444444444444444444444444444444444444444444444
-- 050:3333333333333333333333333333333333333333333333333333333333333333
-- 051:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 052:00000ccc00000ccc00000ccc00000ccc00000ccc00000ccc00000ccc00000ccc
-- 053:ccc00000ccc00000ccc00000ccc00000ccc00000ccc00000ccc00000ccc00000
-- 054:4444444422222222444444442222222222222222222222222222222222222222
-- 055:4444444444444444222222224444444422222222222222224444444422222222
-- 056:cccccccc0ccccccc00cccccc000ccccc0000cccc00000ccc000000cc0000000c
-- 057:ccccccccccccccc0cccccc00ccccc000cccc0000ccc00000cc000000c0000000
-- 058:6666666666666666666666666666666666666666666666666666666666666666
-- 059:000000cc00000ccc000000cc0000000c00000000000000000000000000000000
-- 060:cccc0000ccccc000cccc0000cc00000000000000000000000000000000000000
-- 062:ccccc00000ccc000000000000000000000000000000000000000000000000000
-- 063:cccc0000cccc0000000000000000000000000000000000000000000000000000
-- 064:2222222222222222222222222222222222222222222222222222222222222222
-- 065:5555555555555555555555555555555555555555555555555555555555555555
-- 066:6666666666666666666666666666666666666666666666666666666666666666
-- 067:7777777777777777777777777777777777777777777777777777777777777777
-- 068:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 069:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 071:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 072:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 074:00000000000000000000000000000000000000cc0000cccc0000cccc00cccccc
-- 075:000000000000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00
-- 077:0000000000000000000000000000000000000000000000cc0000cccc0000cccc
-- 078:00000000000000000000000000000000ccccc000ccccccc0ccc00c00ccccc000
-- 080:8888888888888888888888888888888888888888888888888888888888888888
-- 081:9999999999999999999999999999999999999999999999999999999999999999
-- 082:1111111111111111111111111111111111111111111111111111111111111111
-- 083:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
-- 084:0000009900999999009999990099999000000999000009990000000900999999
-- 085:9009000090099990999999000999990090000000999990999900099999990099
-- 087:0000009900999999009999990099999000000999000009990000000900000999
-- 088:9009000090099990999999000999990090000000999990009900000099990000
-- 090:00cccccc00000099009999990099999900999990000009990000099900000009
-- 091:cccccccc90099000900999909999990009999900900000009999909999000999
-- 093:00cccccc00cccccc000000990099999900999999009999900000099900000999
-- 094:ccccccc0cccccccc900900009009999099999900099999009000000099999000
-- 096:ccccccccbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbcccccccc
-- 097:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 098:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
-- 099:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 100:0099999900900999009990090099099900000ccc00000ccc00cccccc00ccc000
-- 101:99990099999999999999999999990000cccc0000cccc0000c00c0000000ccccc
-- 103:0009999909999999099999990009999900000ccc00000ccc00000ccc0000ccc0
-- 104:99990000999999009999990099990000cccc0000cccc0000c00ccc00000ccc00
-- 106:009999990099999900900999009990090099099900000ccc00000ccc00cccccc
-- 107:9999009999990099999999999999999999990000cccc0000cccc0000c00c0000
-- 109:00000009000009990009999909999999099999990009999900000ccc000ccccc
-- 110:990000009999000099990000999999009999990099990000cccc0000cccc0000
-- 112:4444444444444444444444444444444444444444444444444444444444444444
-- 113:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
-- 114:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 115:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 116:0000cccc0000cccc000000000000000000000000000000000000000000000000
-- 117:000ccccc000ccc00000000000000000000000000000000000000000000000000
-- 119:0000ccc000cccc0000cccccc0000cccc00000000000000000000000000000000
-- 120:0ccc00000cccccccc00cccccc000000000000000000000000000000000000000
-- 122:00ccc0000000cccc0000cccc0000000000000000000000000000000000000000
-- 123:000ccccc000ccccc000ccc000000000000000000000000000000000000000000
-- 125:000ccccc00000ccc00000ccc0000000000000000000000000000000000000000
-- 126:00000000cc000000cccc00000000000000000000000000000000000000000000
-- 128:bbbbcbbbbbbcbbbbbbbcbbbbbbbbcbbbbbbbbcbbbbbbbcbbbbbbbcbbbbbbcbbb
-- 129:3333c333333c3333333c33333333c33333333c3333333c3333333c333333c333
-- 130:0000009900999999009999990099999900000999000009990000000900999999
-- 131:9009000090099990999999009990000090099900999900009900000099990000
-- 132:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 133:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 135:0000000000000000000ccccc0ccccccc0ccc00cc0000cccc00cccccccccccccc
-- 136:000000000000000000000000cc000000cccc0000cccc0000cccccc00cccccc00
-- 137:000000000000000000000000000000000000000000000000000000000000000c
-- 138:0000000000000000000000cc000ccccc00cccc000cc00000cc000000c0000000
-- 139:0000000000000000cc000000cccc000000cccc000000ccc0000000cc0000000c
-- 140:000000000000000000000000000c000000cc00000ccc0000cccc0000cccc0000
-- 141:00bbbb000bbbbbb0bbbbbbbb0bbbbbbb00bbbbbb000bbbbb0000bbbb000000bb
-- 142:00bbbb000bbbbbb0bbbbbbbbbbbbbbb0bbbbbb00bbbbb000bbbb0000bb000000
-- 144:bbbbcbbbbbbbcbbbbbbbbcbbbbbbbcbbbbbbbbcbbbbbbcbbbbbbbcbbbbbbbcbb
-- 145:5555555555555555555555555555555555555555555555555555555555555555
-- 146:0099999900990999009909990099099900000ccc00000ccc00000ccc0000000c
-- 147:99990000999900009999000099990000cccc0000cccc0000ccccccc0ccccccc0
-- 148:0000009900999999009999990099999000000999000009990000000900000999
-- 149:9009000090099990999999000999990090000000999990009900000099990000
-- 151:0000900909999009009999990000000900000009990999999990009999000099
-- 152:9222222099992220999922229999222299902222999022999000299990000099
-- 153:000000cc00000cc0000000000000000000000000000000000000000000000000
-- 155:000000cc00000ccc0000cccc0000000000000000000000000000000000000000
-- 156:cccc0000cccc0000cccc00000000000000000000000000000000000000000000
-- 157:c0000000cc000000ccc00000cccc0000ccccc000cccccc00ccccccc0cccccccc
-- 160:bbbbbbccbbbbbbcbbbbbbccbbbbbbcbbbbbbccbbbbbbcbbbbbbbcbbbbbbbcbbb
-- 161:bbbcbbbbbbbcbbbbbbbcbbbbbbbcbbbbbbbcbbbbbbccbbbbcccbbbbbcbbbbbbb
-- 164:0009999909999999099999990009999900000ccc000ccccc000ccccc00000ccc
-- 165:99990000999999009999990099990000cccc0000cccc000000000000cc000000
-- 167:99009999999999990000cccc000ccccc00cccccc000099000000999000000ccc
-- 168:9999009999999999ccc00000cccc0000ccccc00099000000999000000ccc0000
-- 170:0000000c000000cc00000ccc0000cccc000ccccc00cccccc0ccccccccccccccc
-- 171:0000000000000000000000000000000000000000cccccccccccccccccccccccc
-- 173:ccccccccccccccc0cccccc00ccccc000cccc0000ccc00000cc000000c0000000
-- 176:bbbbcbbbbbbbcbbbbbbbcbbbbbbbcbbbbbbbcbbbbbbbccbbbbbbbcccbbbbbbbc
-- 177:ccbbbbbbbcbbbbbbbccbbbbbbbcbbbbbbbccbbbbbbbcbbbbbbbcbbbbbbbcbbbb
-- 178:5555588855555888555558885588888855888888558888888888888888888888
-- 179:8888888888888888888888888888885588888855888888558885555588855555
-- 180:00000ccc00000000000000000000000000000000000000000000000000000000
-- 181:cccc000000000000000000000000000000000000000000000000000000000000
-- 183:00000cc00000ccc0000000000000000000000000000000000000000000000000
-- 184:0cc00000ccc00000000000000000000000000000000000000000000000000000
-- 186:cccccccc0ccccccc00cccccc000ccccc0000cccc00000ccc000000cc0000000c
-- 187:cccccccccccccccccccccccc0000000000000000000000000000000000000000
-- 189:000000000000000000000000000000000000000000000000000000cc0000cccc
-- 190:0000000000000000000000000000000000000000ccccc000ccccccc0cc00ccc0
-- 192:88888888dddddddd99999999acccccaacccccccbcccccccccccccccc3ccccccc
-- 193:88ccccccdccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 194:cccc8888cccccdddcccccc99cccccaaaccccbbbbcccdddddccccccbbccccccc3
-- 195:ff000000ff000000ff00000fff000fffff00ffff00ffffff0fffffffffffffff
-- 196:000000ff000000fff00000fffff000ffffff00ffffffff00fffffff0ffffffff
-- 197:00000fff0000f00000ff00ffff000fff00f0ffff0fffffffffffffffff00ffff
-- 198:fff00000000f0000ff00ff00fff000ffffff0f00fffffff0ffffffffffff00ff
-- 199:000fff0000f00000ff00000fff000fffff00ffff00ffffff0fffffffffffffff
-- 200:00fff00000000f00f00000fffff000ffffff00ffffffff00fffffff0ffffffff
-- 201:000000000000000000000000000000cc0000cccc0000cccc00cccccc00cccccc
-- 202:0000000000000000ccccc000ccccccc0cc00ccc0cccc0000cccccc00cccccccc
-- 203:0000000000000000000ccccc0ccccccc0ccc00cc0000cccc00cccccccccccccc
-- 204:000000000000000000000000cc000000cccc0000cccc0000cccccc00cccccc00
-- 205:0000cccc00cccccc00cccccc0000009900999999009999990099999900000099
-- 206:cccc0000cccccc00cccccccc9009900090099000999999909999990090000000
-- 208:dddddccc3333cccc33ccccccbccccccc3ccccccc3cccccccddcccccc33333333
-- 209:ccccccccccccccccccccccccccccccccccccccccccc3ccccccddddcc33333333
-- 210:cccccccdccccc333cccc3333ccccccbbccccccc3cccccccccccccccd3ccccc33
-- 211:ff0fffffff0fffffff00ffffff000fffff00f0ffff00000f00f00000000fff00
-- 212:fffff0fffffff0ffffff00fffff000ffff0f00fff00000ff00000f0000fff000
-- 213:ff00ffffff0000ffff00ff00ff000000ff000000ff000000ff000000ff000000
-- 214:ffff00ffff0000ff00ff00ff000000ff000000ff000000ff000000ff000000ff
-- 215:ff00ffffff0000ffff00ff00ff000000ff000000ff00000000f00000000fff00
-- 216:ffff00ffff0000ff00ff00ff000000ff000000ff000000ff00000f0000fff000
-- 217:0000009900999999009999990099999000000999009909990099900900990009
-- 218:9009000090099990999999000999990090000000999990009900000099990099
-- 219:0000900909999009009999990099999000000009000999990000009999099999
-- 220:9222222099992220999922220999222299902222999022999000299999900099
-- 221:0099999900cccccc00cccccc00cccccc000000cc000000cc0000cccc0000cccc
-- 222:99999900c0000000c0000000ccccc000ccccc000ccccc000c0000000cccc0000
-- 230:0000000000000000000000000000000000cccc000cc00cc00cccc000ccccccc0
-- 233:0099999900099999000009990000099900000ccc00000ccc00cccccc00ccc000
-- 234:99999999999999909999000099990000cccc0000cccc00000ccc00000ccc0000
-- 235:99099999099999990000cccc000ccccc00cccccc000099000000999000000ccc
-- 236:9999009999999999ccc00000cccc0000ccccc000999000000999000000ccc000
-- 237:bbbbbbbbbbbbbbbb44444444bbbbbbbbbbbbbbbbbbbbbbbb44444444bbbbbbbb
-- 240:8888888888888888888888888888888888888888888888888888888888888888
-- 241:6666666666666666666666666666666666666666666666666666666666666666
-- 242:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 243:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
-- 244:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- 245:3333333333333333333333333333333333333333333333333333333333333333
-- 246:0999090099990999990999900990000000999900000000000000000000000000
-- 247:5555888855558888555588885555888855558888888888888888888888888888
-- 248:8888888888888888888888888888888888888855888888558888555588885555
-- 249:0000cccc0000cccc000000000000000000000000000000000000000000000000
-- 250:00cccc0000cccc00000000000000000000000000000000000000000000000000
-- 251:00000cc00000ccc0000000000000000000000000000000000000000000000000
-- 252:00cc00000ccc0000000000000000000000000000000000000000000000000000
-- 253:4444444444444444bbbbbbbb444444444444444444444444bbbbbbbbbbbbbbbb
-- 254:4444444444444444444444444444444444444444bbbbbbbb4444444444444444
-- </TILES>

-- <MAP>
-- 000:262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161363616163636363636163636361636363616163636361636363616363636363636363636363636363636363636363636363636363636363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 001:262614141426262626262626262660602626262626261414262626262626262614142626262626262626262660602626262626261426141426261426626262626262620c1c2c626262626262626262626262620c1c2c62626262626262626262626262626262626262626262626262626262626262626262361616163636161636161636161616163616161636161616361616361636363636363636363636363636363636363636363636363636363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 002:261414141426262614142626609060606060141426141414141414262626261414141414262614142614609060606060141426141426141414261414717171717171710d1d2d717171717171717171717171710d1d2d71717171717171262626262671717171262626262671717171712626267171717171161616161616161616161616161633171616161616161616161616161616363636363636363636363636363636363636363636363636363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 003:141414141414141414901490609060606060601414149014909090269026141414141414142614901490609060606060601414149014909090149014727272262626262672727272727272727272727272727272727272262626722626262626262672722626262626262626727272262626262672727226331633161633161633161616333333331716163333161633163316163316363636363636363636363636363636363636363636363636363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151d8e851515151515151515151515151
-- 004:9014909090149014149090909090609060909060901490149090902690269014909090149014149090909090609060909060901490149090901490147326262626262626262673737373732626262626737373737326262626266326262626262626262626262626262626262663262626262626266326263333333316331616333316163333331733173333331633333333161633333636363636363636363636363636363636363636363636363636363636365252525252525252525252525252525252525252525252525252525252525151515151515151515151519cac51515151bccc51515151515151515151
-- 005:9090909090909060606060906060609090909090906090909090909090609090909090909060606060906060609090909090906090909090909090602626262626262626262626266363262626262626266363632626262626262626262626262626262626262626262626262626262626262626262626263333333333333316333333163333173317333333333333333333331633333636040404040404043636363636363636363636360404040404040436365252525252525252525252525252525252525252525252525252525252525151515151515151515151519dad51515151bdcd51515151515151515151
-- 006:808090909080808080808090808080808090908080808080909090808080808090909080808080808090808080808090908080808080909090808080262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626333333333333333333333333333333173333333333333333333333163333363636040404040436363636365116164f4f4f4f4f4f04040404043636365252525252525252525252525252525252525252525252525252525252525151515151515151515151519eae51515151bece51515151515151515151
-- 007:dededede908080808080809080808080809090808080808090909080808023232525258080808080809080808080809090808080808090909080808026262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262633333333333333333333333333330817333333333333333333333333333336363636363636363636363636512f2f4f4f4f4f4f4f4f4f3636363636365252525252525252525252525252525252525252525252525252525252525151515151515151515151519faf51515151bfcf51515151515151515151
-- 008:dfdfdfdfdfdf80808080809080808080809090808080808090909080808023232525252323808080809080808080809090808080808090909080808026262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262633333333333333333333333333330833333333333333333333333333333336363636363636363636363636512f2f4f4f4f4f4f4f4f4f363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 009:efefefefefefef808080809080808080809090808080808090909080808023232525252323808080809080808080809090808080808090909080808026262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262633333333333333333333333333330817333333333333333333333333333336363636363636363636361616161616162f2f4f4f4f4f4f363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 010:dedededededede808080809080808080809090808080808090909080808023232525252323802380809080808080809090808080808090909080808026262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262633333333333333333333333333330933333333333333333333333333333336363636363636363636362f363616163636164f4f4f4f4f363636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 011:0404040404048080808080908080808080909080808080809090908080802323252525238080238080908080808080909080808080809090908080805050505050505050505050502b0505053b5050505050505050505050505026262626262626262626262626262626262424242424242424242424242423232323232323232323232323231823232323232323232323232323232336363636361616161616362f3616161616362f362f2f2f2f163636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 012:04040101040423232323232523232323232525232323232325252523232323232525252323232323232523232323232525232323232325252523232350505050505050505050502b0505053b505050505050505050505050505026262626262626262626262626262626261f24242424242424242424242416161616165f2323232323232323182323232323232323235f161616161636363636362f1616162f4f2f3636161636362f4f2f161616163636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 013:040401010404242424242425242424242424242424242424252525242424242425252522a12424242425242424242424242424242424252525242424505050505050505050502b0505053b50505050505050505050505050505026262626262626262626242424242424242424242424242424242424242416161616162f5f232323232323231823232323232323235f2f161616161636363636362f3636362f4f2f1636161636162f4f2f363636163636363636525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 014:04040404040451515151515151515151515151515151515151515151515136363636363f81913f5151515151515151515151515151515151515151515050505050505050507f0f0f0f8f50505050505050505050505050505050262626262626262626261f241414142424242424141424242424142424241616161616162f5f23232323232323232323232323235f2f16161616161605050505050f0f05050f0f0f0f0505050505050505050505050505050505525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 015:04040404040460606060606060606060606060606060606060606060606024242424243f92a23f606060606060606060606060606060606060606060050505050505050505050505050505050505050505050505050505050505242424242424242424242414141414142424141414141424241414142414161616161616162f2323232323235f5f2323232323232f16161616161616153615151515361515151515361515151536151515151536151515153615525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 016:60606060606060606060606060606060606060606060606060606060606060606060606060a33f606060606060606060606060606060606060606060050505050505050505050505050505050505050505050505050505050505141414141414242424141414141414141414141414141414141414141414161616161616161616161616161616161616161616161616161616161616363535353536353535353535363535353536353535353535363535353535525252525252525252525252525252525252525252525252525252525252515151515151515151515151515151515151515151515151515151515151
-- 017:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 018:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 019:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 020:828202020202020202020202020202020202020202020202020202028282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 021:828202020202020202020202020202020202020202020202020202028282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 022:828202020202020202020202020202020202020202020202020202020282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 023:8282b0c08282828282828282828282828282828282828282828243538282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 024:8282b1c18282828282828282828282828282828282828282828283938282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 025:8282b2c282828282827484828252e0f082828282828244548282dbeb8282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 026:8282b3c382828282827585525252e1f1aaba8252bad945558282dcec8282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 027:828232428298a8b8c87686525252e2f2abbb8252bbda46568282dded8282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 028:82824353829982b9c97787828252e3f38282828282824757828252528282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 029:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 030:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 031:828282828282828282828282828282828282828282828282828282828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 032:828282828282828282828282828282828282828282820000000082828282515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 033:000000000000000000000000000000000000000000000000000000000000515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 034:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 035:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 036:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 037:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 038:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 039:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 040:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 041:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 042:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 043:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 044:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 045:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 046:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 047:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 048:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 049:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 050:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 051:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 052:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 053:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 054:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 055:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 056:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 057:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 058:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 059:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 060:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 061:515151515151515151515151515151515151515151515151515151515151515151515151515151825151515182515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 062:515151515151515151515151515151515151515151515151515151515151131313131351515151518282825173839351515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 063:5151515151515151515151515151515151515151515151515151515151511313131313515151515182825151748494a4b451d4e45151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 064:5151515151515151515151515151515151515151515151515151515151511313131313515151515182828282758595a5b551d5e55151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 065:5151515151515151515151515151515151515151515151515151515151511313131313515151515182820082768696a6b651d6e65151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 066:5151515151515151515151515151515151515151515151515151515151512424242424242424242424242482778797a7b782d7e72424242424242424515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 067:515151515151515151515151515151515151515151515151515151515151242424242424242424242424242424242424242424242424242424242424515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 068:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 069:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 070:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 071:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 072:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 073:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 074:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 075:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 076:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 077:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 078:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 079:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 080:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 081:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 082:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 083:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 084:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 085:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 086:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 087:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 088:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 089:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 090:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 091:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 092:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 093:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 094:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 095:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 096:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 097:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 098:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 099:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 100:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 101:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 102:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 103:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 104:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 105:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 106:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 107:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 108:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 109:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 110:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 111:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 112:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 113:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 114:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 115:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 116:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 117:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 118:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 119:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 120:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 121:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 122:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 123:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 124:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 125:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 126:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 127:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 128:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 129:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 130:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 131:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 132:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 133:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 134:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- 135:515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151
-- </MAP>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:018f018f018f716f9162a18fa16db16fc165d15ff19fb11fa11fa12d91b4812c717c712c012da12d014da15d016db18d018db10cb10f010301020100561000000f00
-- </SFX>

-- <TRACKS>
-- 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </TRACKS>

-- <FLAGS>
-- 000:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </FLAGS>

-- <PALETTE>
-- 000:041c306d4430f6fa55ffaafffa99652cb6794c79280004ee2936ba4c59ff41a6f69d006df4f4f49150c61c9944333c57
-- 001:040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </PALETTE>

