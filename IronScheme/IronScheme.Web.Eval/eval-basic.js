function docall() {
  $.post('basic.ss', { expr: $("#expr").val() },
      function (data) {
        $("#result").html(data);
      });
}

$("#eform").submit(function (event) {
  $("#result").text('Please wait...');
  event.preventDefault();
  docall();
});


