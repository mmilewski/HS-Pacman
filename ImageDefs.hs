module ImageDefs where

img_pacman = "pacman"
img_pacman_hunting = "pacman_hunting"
img_enemy = "enemy"
img_ball = "ball"
img_fruit = "fruit"
img_board_empty = "board-empty"
img_board_bottom = "board-bottom"
img_board_right = "board-right"
img_board_left = "board-left"
img_board_top = "board-top"
img_btn_play = ["play_on", "play_off"]
img_btn_quit = ["quit_on", "quit_off"]
img_all = [img_fruit,
           img_ball,
           img_enemy,
           img_pacman, img_pacman_hunting,
           img_board_empty, img_board_bottom, img_board_right, img_board_left, img_board_top]
          ++ img_btn_play
          ++ img_btn_quit
