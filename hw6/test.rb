class Square
    def initialize(side_length=0)
        @side_length = side_length
        if defined?(@@counter) then @@counter += 1 else @@counter = 1 end
    end
    def area
        @side_length * @side_length
    end 
    def self.hello
        puts "Hello from class method!"
    end
    def Square.hello1
        puts "Hello from another class method!"
    end
    def Square.count
        @@counter
    end

end


s1 = Square.new(10)
puts s1.area
s2 = Square.new
puts s2.area
puts Square.count
