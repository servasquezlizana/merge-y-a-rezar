module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test Punto 1: Valor de una ciudad" $ do
    it "Valor de la ciudad 'Baradero', fundada en 1615, cuyas atracciones son Parque del Este y Museo Alejandro Barbich, con un costo de vida de 150" $ do
      valorCiudad (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150) `shouldBe` 925
    it "Valor de la ciudad Nullish, fundada en 1800, sin atracciones y un costo de vida de 140" $ do
      valorCiudad (Ciudad "Nullish" 1800 [] 140) `shouldBe` 280
    it "Valor de la ciudad Caleta Olivia, fundada en 1901, cuyas atracciones son El Gorosito y Faro Costanera, y un costo de vida de 120" $ do
      valorCiudad (Ciudad "Caleta Olivia" 1901 ["El Gorosito", "Faro Costanera"] 120) `shouldBe` 360

  describe "Test Integrante 2: Ciudad Sobria" $ do
    it "Valor de la ciudad Baradero, fundada en 1615, cuyas atracciones son Parque del Este y Museo Alejandro Barbich, con un costo de vida de 150" $ do
      sobriaCiudad (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150) 14 `shouldBe` True
    it "Valor de la ciudad Baradero, fundada en 1615, cuyas atracciones son Parque del Este y Museo Alejandro Barbich, con un costo de vida de 150" $ do
      sobriaCiudad (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150) 15 `shouldBe` False
    it "Valor de la ciudad Caleta Olivia, fundada en 1901, cuyas atracciones son El Gorosito y Faro Costanera, y un costo de vida de 120" $ do
      sobriaCiudad (Ciudad "Caleta Olivia" 1901 [] 120) 5 `shouldBe` False

  describe "Test grupal 2: agregar nueva atraccion " $ do
    it "Valor de la ciudad Baradero, fundada en 1615, cuyas atracciones son Parque del Este y Museo Alejandro Barbich, con un costo de vida de 150" $ do
      sumarAtraccion (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150) ["Roma"] `shouldBe` (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich", "Roma"] 180)

  describe "Test grupal 2: agregar nueva atraccion " $ do
    it "Valor de la ciudad Baradero, fundada en 1615, cuyas atracciones son Parque del Este y Museo Alejandro Barbich, con un costo de vida de 150" $ do
      remodelacion (Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 200) "New Baradero" `shouldBe` (Ciudad "New Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 300)
  



  