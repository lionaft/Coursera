# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[0, 0], [-1, 0], [1, 0], [0, 1], [-1, 1]]),
                  [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
                  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, 1], [1, 0]])]


  # your enhancements here
  def move (delta_x, delta_y, delta_rotation)
    # Ensures that the rotation will always be a possible formation (as opposed 
    # to nil) by altering the intended rotation so that it stays 
    # within the bounds of the rotation array
    moved = true
    potential = @all_rotations[(@rotation_index + delta_rotation) % @all_rotations.size]
    # print "#{@all_rotations[@rotation_index]} - #{potential}\n"
    # for each individual block in the piece, checks if the intended move
    # will put this block in an occupied space
    potential.each{|posns| 
      if !(@board.empty_at([posns[0] + delta_x + @base_position[0],
                            posns[1] + delta_y + @base_position[1]]));
        moved = false;  
      end
    }
    if moved
      
      @base_position[0] += delta_x
      @base_position[1] += delta_y
      @rotation_index = (@rotation_index + delta_rotation) % @all_rotations.size
      
    end
    moved
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.rotations2(point_array)
    rotate1 = point_array.map {|x,y| [-y,x]}  
    rotate2 = point_array.map {|x,y| [x,y]} 
    rotate3 = point_array.map {|x,y| [y,-x]}  
    [point_array, rotate1, rotate2, rotate3]  
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.count-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat
    if !@cheat and @score >= 100
      @cheat = true
      @score -= 100
    end
  end

end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    @root.bind('n', proc {self.new_game}) 

    @root.bind('p', proc {self.pause}) 

    @root.bind('q', proc {exitProgram})
    
    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left}) 
    
    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right}) 

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise}) 

    @root.bind('u', proc {@board.rotate_180_degrees})

    @root.bind('c', proc {@board.cheat})
    
    @root.bind('space' , proc {@board.drop_all_the_way}) 
  end
end


