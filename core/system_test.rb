#! /opt/local/bin/ruby -w
# -*- mode:ruby; coding:utf-8 -*-
require 'open3'

def send(client, send, *expects)
  stdin,stdout,stderr = *client
  stdin.puts send
  puts send
  expects.each do|expect|
    actual = stdout.readline.strip
    puts "==> #{actual}"
    if actual == expect then
      puts "ok"
    else
      raise "error: expected #{expect.inspect} but got #{actual.inspect}"
    end
  end
end

begin
  system "./dominion -debug &"
  Open3.popen3("./dominion -shell") do|*c1|
    Open3.popen3("./dominion -shell") do|*c2|
      send c1,"/ls","0"
      send c1,"/make hole","ok"
      send c1,"/ls",'["hole"]'
      send c2,"/ls",'["hole"]'

      send c1, "/join hole alice", "ok"
      send c2, "/join hole rabbit", "ok"
      send c1, "/ready", "ok"
      send c2, "/ready", "start@hole", "alice's turn@hole", "alice's action phase@hole", "ok"

      send c1, "/skip", "alice's buy phase@hole", "ok"
      send(c1, "/skip",
           "alice's clean phase@hole",
           "rabbit's turn@hole",
           "rabbit's action phase@hole",
           "ok")

      send c1, "/skip", "rabbit's buy phase@hole", "ok"
      send(c1, "/skip",
           "rabbit's clean phase@hole",
           "alice's turn@hole",
           "alice's action phase@hole",
           "ok")
    end
  end
ensure
  system "pkill dominion > /dev/null 2>&1"
end
