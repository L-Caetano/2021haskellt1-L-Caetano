import Text.Printf
import Data.Fixed

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)


-------------------------------------------------------------------------------
--Mude o tamanho das variaveis col e lin para criar uma imagem nova
-------------------------------------------------------------------------------

col :: Int 
col = 9

lin :: Int 
lin = 9

-----------------------------------------------------------------
---------------------------------------------------------------
palette :: Int -> Int -> [(Int,Int,Int)]
palette n color= 
  if color == 1  then [(70+i*10,0,0) | i <- [0..(n-1)] ]
  else if color == 2 then [(0,70+i*10,0) | i <- [0..(n-1)] ] 
  else [(0,0,70+i*10) | i <- [0..(n-1)] ]
------------------------------------------------------------------

rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,0,255),(0,255,0)]


-------------------------------------------------------------------------------

svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s'/>" x y w h style

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

rectAlinhar :: Int -> Int -> [Rect]
rectAlinhar n c =
  [((m*(w+gap),(fromIntegral c)*(w+(2*gap))),w,w) | m <- [0..fromIntegral (n-1)]]
    where 
         gap = 
          if odd(n) && odd(c) then 2
          else if even(n) && odd(c) then 3
          else 4
         w = 
          if odd(n) && odd(c) then 100
          else if odd(n) && even(c) then 200
          else 300 
-----------------------------------         
linhasRect :: Int -> Int -> [String]
linhasRect lin col = 
  ["  " ++ svgRect (last (rectAlinhar l c)) (svgStyle (last (palette(l+(color*c)) color)))  | l <- [1..lin], c <- [col]]
    where 
          color =
            if odd(lin) && odd(col) then 1
            else if odd(lin) && even(col)  then 2
            else 3
-------------------------
colunasRect :: Int -> Int -> [[String]]
colunasRect lin col =  
  [linhasRect l c | l <- [lin], c <- [0..(col-1)]]

printRects :: Int -> Int -> String
printRects l c = 
  printf (unlines $ (concat (colunasRect l c)))

-------------------------------------------------------------------------------

genImage :: IO()
genImage = do
  writeFile "image1.svg" $ svgstr
  where svgstr = svgBegin w h ++ printRects col lin ++ printRects (col-5) (lin-5) ++ svgEnd
        (w, h) = (1500, 1500)
      
       
-------------------------------------------------------------------------------
-- Executa todos os casos
-------------------------------------------------------------------------------

main :: IO()
main = do 
  genImage