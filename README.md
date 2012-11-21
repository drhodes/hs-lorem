Generate lorem ipsum for your haskell project




import qualified Text.Lorem as TL

-- generate a random paragraph between 3 and 5 sentences long
main = do txt <- TL.paragraph 3 5 
          print txt

>> "Nos istuc amplitudines fuisse consuetudinis tamdiu manu 
>> instat illic, mira motus, atque e alibi caste huc id commendavi
>> auris. Scirent subditus turibulis vult fit interrogare en. 
>> Dum multiplices dicere hae latis a habiti valida quaerit palliata 
>> ipso ambiendum ex os ne nam tam eo impium tuus dinoscens."