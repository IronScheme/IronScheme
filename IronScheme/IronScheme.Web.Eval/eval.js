$("#eform").submit(function(event) {
  event.preventDefault();
  $.post('', { expr: $("#expr").val() },
      function(data) {
        $("#result").html(data);
      });
});