#! /opt/local/bin/ruby -w
# -*- mode:ruby; coding:utf-8 -*-
require 'open3'

def send(client, send, expect)
  stdin,stdout,stderr = *client
  stdin.puts send
  actual = stdout.readline.strip
  if actual == expect then
    puts "ok"
  else
    puts "error: expected #{expect.inspect} but got #{actual.inspect}"
  end
end

system "./dominion &"
Open3.popen3("./dominion -shell") do|*c1|
  Open3.popen3("./dominion -shell") do|*c2|
    send c1,"/ls","0"
    send c1,"/make hole","ok"
    send c1,"/ls",'["hole"]'
    send c2,"/ls",'["hole"]'
  end
end
system "pkill dominion"
