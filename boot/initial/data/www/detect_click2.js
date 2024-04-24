$(document).ready(function(){
    $('body').on('click', 'a', function(evt){
        Shiny.setInputValue('file_tree_1-clicked_text', evt.target.id);
    });
})
