<div class="panel panel-info" style="margin-top: 30px;">
    <div class="panel-heading">
          <div class="panel-title">Login</div>
    </div>
    <div class="panel-body">
        <div>
            <dfForm action="/login" role="form">
            <dfIfChildErrors ref="">
            <div class="alert alert-dismissable alert-danger">
            <dfChildErrorList ref="" />
            </div>
            </dfIfChildErrors>
            <div class="form-group">
                 <dfLabel ref="name">Username</dfLabel>
                 <dfInputText class="form-control" ref="name"/>
                 <dfLabel ref="password">Password</dfLabel>
                 <dfInputPassword class="form-control" ref="password"/>
                 <dfInputSubmit class="btn btn-lg btn-success btn-block" value="Login"/>
            </div>
            </dfForm>
        </div>
    </div>
</div>