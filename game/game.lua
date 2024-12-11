-- title:   game title
-- author:  game developer, email, etc.
-- desc:    short description
-- site:    website link
-- license: MIT License (change this to your license of choice)
-- version: 0.1
-- script:  lua

local _ENV = require 'std.strict' (_G)

local class = require 'middleclass'
local Vector2 = class('Vector2')
local Enum = require('enum')
function Vector2:initialize(x, y)
   self.x = x or 0
   self.y = y or 0 
end

function Vector2:add(o)
   self.x = self.x + o.x
   self.y = self.y + o.y
end

local t = 0
local v = Vector2:new(96, 24)
local up = Vector2:new(0, -1)
local down = Vector2:new(0, 1)
local left = Vector2:new(-1, 0)
local right = Vector2:new(1, 0)
local points = Enum("fence_ponits   0", "water_points   0")
local test = -1
local points = {
   fence = 0,
   water = 0
}
function TIC()
   if btn(0) then 
      v:add(up) 
      trace(points.fence)
      points.fence=1
      trace(points.fence)
   end
   if btn(1) then v:add(down) end
   if btn(2) then v:add(left) end
   if btn(3) then v:add(right) end

   cls(13)
   spr(1+t%60//30*2,v.x,v.y,14,3,0,0,2,2)
   print("HELLO WORLD!",84,84)
   t=t+1
end

-- <TILES>
-- 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
-- 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
-- 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
-- 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
-- 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- </TILES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
-- </SFX>

-- <TRACKS>
-- 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </TRACKS>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

