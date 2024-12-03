      var darkmodeEnabled = false;

      function toggleDarkMode() {
        $('body').find('.ui').addClass('inverted');
        $('body').addClass('inverted');
        $('#darkmode > i').removeClass('moon').addClass('sun');
        darkmodeEnabled = true;
      }

      function toggleLightMode() {
        $('body').find('.ui').removeClass('inverted');
        $('body').removeClass('inverted');
        $('#darkmode > i').removeClass('sun').addClass('moon');
        darkmodeEnabled = false;
      }

      $(document).on('click', '#darkmode', function () {
        if (darkmodeEnabled) {
          toggleLightMode();
        } else {
          toggleDarkMode();
        }
      });