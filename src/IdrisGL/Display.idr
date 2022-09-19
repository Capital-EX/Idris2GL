{- Tian Z (ecburx@burx.vip) -}

||| Display a picture in window.
module IdrisGL.Display

import System
import IdrisGL.Picture
import IdrisGL.DataType
import IdrisGL.Color
import IdrisGL.SDL.SDL_event
import IdrisGL.SDL.SDL_video
import IdrisGL.SDL.SDL_render
import IdrisGL.SDL.SDL_surface
import IdrisGL.SDL.SDL_timer

||| Open a new window and display the given picture.
|||
||| @ window  Display mode.
||| @ bgColor Background color.
||| @ pic     Picture.
export
display : (window  : Display)
       -> (bgColor : Color)
       -> (pic     : Picture) 
       -> IO ()
display window bgColor pic  =  do 
    win                     <- createWin window
    ren                     <- createRenderer win
    setRenderDrawColor         ren bgColor

    renderClear                ren
    loadPicture                pic ren win
    renderPresent              ren
    updateWinSur               win
    e                       <- newEve
    loop                       pic ren win bgColor e

    closeWin                   win
    freeEve e
    freeRender                 ren

    where loop : Picture -> Renderer -> Win -> Color -> Event -> IO ()
          loop pic ren win bgColor e with (eveType e)
              loop _   _   _   _ _       | E_QUIT = pure ()
              loop pic ren win bgColor e | _      = 
                    do setRenderDrawColor ren bgColor
                       renderClear        ren 
                       loadPicture        pic ren win
                       renderPresent      ren
                       usleep             1000000
                       loop               pic ren win bgColor e