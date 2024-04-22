$(document).ready(function(){
    $('body').on('click', 'a', function(evt){
        Shiny.setInputValue('fileTree_1-clicked_text', evt.target.id);
    });
    })