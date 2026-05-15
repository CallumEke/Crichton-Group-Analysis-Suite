$(document).on('dragover', '.shiny-input-container .input-group', function(e) {
  e.preventDefault(); $(this).addClass('drag-over');
});
$(document).on('dragleave drop', '.shiny-input-container .input-group', function(e) {
  $(this).removeClass('drag-over');
});
