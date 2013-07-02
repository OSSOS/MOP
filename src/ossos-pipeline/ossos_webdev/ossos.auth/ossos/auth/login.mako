<%inherit file='base.mako' />

% if failed_attempt:
<p><font color="red">Invalid credentials, try again.</font></p>
% endif

<form class="form-horizontal" method="post" action="${ request.path }">
  <div class="control-group">
    <label class="control-label" for="login">CADC username</label>
    <div class="controls">
        <input type="text" name="login" value="${ login }">
    </div>
  </div>
  <div class="control-group">
    <label class="control-label" for="passwd">Password</label><br>
    <div class="controls">
      <input type="password" name="passwd">
    </div>
  </div> <!-- control-group -->
  <div class="control-group">
    <div class="controls">
      <input type="hidden" name="next" value="${ next }">
      <button type="submit" class="btn" name="submit">Sign in</button>
    </div>
  </div> <!-- control-group -->
</form>

