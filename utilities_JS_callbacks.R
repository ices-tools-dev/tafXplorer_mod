callback <- c(
  "$('input[name=rdbtn]').on('click', function(){",
  "  var value = $('input[name=rdbtn]:checked').val();",
  "  Shiny.setInputValue('rdbtn', value);",
  "});"
)

# can maybe replace the detect_click2.js file
#callback2 <- c(
#  "$('body').on('click', 'a.taf-tree-node', function(evt){",
#  "  Shiny.setInputValue('clicked_text', evt.target.id);",
#  "  });"
#)
