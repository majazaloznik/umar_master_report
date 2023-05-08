<script type="text/javascript">
  $(document).ready(function() {
    // Show the table of contents
    $('#TOC').show();

    // Hide the sub-headings by default
    $('#TOC ul ul').hide();

    // Toggle the visibility of the sub-headings when the headings are clicked
    $('#TOC > ul > li > a').click(function(e) {
      e.preventDefault();
      $(this).siblings('ul').toggle();
      // Toggle the 'expanded' class to rotate the triangle
      $(this).parent().toggleClass('expanded');
    });
  });
</script>
