--[[
-- Copyright (c) 2009, Sun Microsystems Inc.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- * Neither the name of Sun Microsystems Inc. nor the names of its contributors
--   may be used to endorse or promote products derived from this software
--   without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
-- BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
-- WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
-- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--]]
local sigar = assert(require("sigar"))

-- test if the GC is called nicely
local s = assert(sigar.new())
s = nil

function print_table(t)
	for k, v in pairs(t) do
		print(("  %s: %s"):format(k, v))
	end
end

local s = sigar.new()

print("-- cpus")
local cpus = s:cpus()

for i = 1, #cpus do
	local cpu = cpus[i]
	print(i)
	print(" info")
	local info = cpu:info()
	print_table(info)
	local data = cpu:data()
	print(" data")
	print_table(data)
end

print("-- mem")
print_table(s:mem())
print("-- swap")
print_table(s:swap())

print("-- procs")
local procs = s:procs()

for i = 1, #procs do
	local pid  = procs[i]
	local proc = s:proc(pid) -- get the data ... but don't use it 

	if pid == s:pid() then
		print(pid)
		print(" mem")
		local mem, msg = proc:mem()
		if mem then print_table(mem) else print("  -- no mem info: " .. msg) end
		print(" time")
		local t, msg = proc:time()
		if t then print_table(t) else print("  -- no time info: " .. msg) end
		print(" state")
		local t, msg = proc:state()
		if t then print_table(t) else print("  -- no state info: " .. msg) end
		print(" exe")
		local t, msg = proc:exe()
		if t then print_table(t) else print("  -- no exe info: " .. msg) end
	end
end

print("-- filesystems")
local fses = s:filesystems()

for i = 1, #fses do
	local fs  = fses[i]

	print(i)
	print(" info")
	local info = fs:info()
	if info then print_table(info) else print("  -- no fs info") end
	print(" usage")
	local usage = fs:usage()
	if usage then print_table(usage) else print("  -- no fs usage") end
end


print("-- disks")
local disks = s:disks()

for i = 1, #disks do
	local disk  = disks[i]

	print(" usage")
	local usage, msg = disk:usage()
	if usage then print_table(usage) else print("  -- no disk usage: " .. msg) end
end

---
-- try to specify a device that is known, but not listed by the fs-list
local disk = s:disk("/dev/disk1")
if disk then
	print(disk:name())
	print(" usage")
	local usage, msg = disk:usage()
	if usage then print_table(usage) else print("  -- no disk usage: " .. msg) end
end


print("-- who")
local who = s:who()

for i = 1, #who do
	local w  = who[i]

	print(" usage")
	local usage, msg = w
	if usage then print_table(usage) else print("  -- no who usage: " .. msg) end
end

print("-- netifs")
local netifs = s:netifs()

for i = 1, #netifs do
	local netif  = netifs[i]

	print(" info")
	local usage, msg = netif:info()
	if usage then print_table(usage) else print("  -- no netif info: " .. msg) end
	print(" usage")
	local usage, msg = netif:usage()
	if usage then print_table(usage) else print("  -- no netif usage: " .. msg) end
end


print("-- version")
print_table(s:version())
print("-- sysinfo")
print_table(s:sysinfo())
