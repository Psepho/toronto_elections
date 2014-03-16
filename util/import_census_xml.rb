#!/usr/bin/ruby
require "rubygems"
require "nokogiri"

file   = ARGV[0]
reader = Nokogiri::XML::Reader(File.open(file))
reader.each do |node|
    next if node.node_type != Nokogiri::XML::Reader::TYPE_ELEMENT
    if node.name =~ /Value/
        puts node.attribute("value")
    end
end