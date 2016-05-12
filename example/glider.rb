
glider = "\
                         #
                       # #
             ##      ##            ##
            #   #    ##            ##
 ##        #     #   ##
 ##        #   # ##    # #
           #     #       #
            #   #
             ##
"

coords = ""

glider.lines.each_with_index do |line, row|
  line.chars.each_with_index do |char, col|
    if char == "#"
      coords << "(#{row} . #{col}) "
    end
  end
end

puts coords
