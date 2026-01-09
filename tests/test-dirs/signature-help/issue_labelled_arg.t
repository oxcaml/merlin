  $ $MERLIN single signature-help -position 2:14 << EOF
  > let f ~x ~y = x + y
  > let _ = 1 - f 
  > EOF
  {
    "class": "return",
    "value": {
      "signatures": [
        {
          "label": "( *type-error* ) : int -> (x:'_weak1 -> y:'_weak2 -> '_weak3) -> 'a",
          "parameters": [
            {
              "label": [
                19,
                22
              ]
            },
            {
              "label": [
                26,
                61
              ]
            }
          ]
        }
      ],
      "activeParameter": 0,
      "activeSignature": 0
    },
    "notifications": []
  }
