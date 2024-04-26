$(document).ready(function(){
    $('body').on('click', 'a.taf-tree-node', function(evt){
        Shiny.setInputValue('clicked_text', evt.target.id);
    });
})
