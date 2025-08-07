module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (orbitalPeriodInSeconds * (multiplier planet))
  where orbitalPeriodInSeconds = 31557600
        multiplier :: Planet -> Float
        multiplier Mercury = 0.2408467
        multiplier Venus = 0.61519726
        multiplier Earth = 1
        multiplier Mars = 1.8808158
        multiplier Jupiter = 11.862615
        multiplier Saturn = 29.447498
        multiplier Uranus = 84.016846
        multiplier Neptune = 164.79132
        