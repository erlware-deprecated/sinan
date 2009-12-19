#!/usr/bin/env ruby

cmd=`git log --pretty='format:%ci::%an <%ae>::%s'`

list = {}
list_order = []

cmd.each do |l|
  date, author, subject = l.chomp.split("::")
  date, time, zone = date.split(" ")

  id = "#{date}\t#{author}"
  if not list[id]
    list[id] = []
    list_order << {:id => id, :value => list[id]}
  end
  list[id] << subject
end

# list.each do |id, value|
list_order.each do |i|
  id = i[:id]
  value = i[:value]

  puts "#{id}"
  puts value.map { |e| "\t* #{e}" }.join("\n")
  puts "\n"
end
