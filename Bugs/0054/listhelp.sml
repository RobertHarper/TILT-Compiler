(*$import *)
datatype 'a list = nil | :: of 'a * 'a list
infixr 5 ::
