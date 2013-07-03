<%inherit file='base.mako' />

% if failed_attempt:
<p><font color="red">Invalid credentials, please try again.</font></p>
% endif

<form class="form-horizontal" method="post" action="${ request.path }">
  <div class="control-group">
    <label class="control-label" for="login">CADC username</label>
    <div class="controls">
        <input type="text" name="login" value="${ login }" 
                autofocus="true">
    </div>
  </div>
  <div class="control-group">
    <label class="control-label" for="passwd">Password</label>
    <div class="controls">
      <input type="password" name="passwd">
    </div>
  </div> <!-- control-group -->
  <div class="control-group">
    <div class="controls">
      <input type="hidden" name="next" value="${ next }">
      <button type="submit" class="btn" name="submit">Sign in</button>
      <a style="margin-left: 40px;s"
         href="http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/cadcbin/auth/userID.pl">Reset password</a>
    </div>
  </div> <!-- control-group -->
</form>

