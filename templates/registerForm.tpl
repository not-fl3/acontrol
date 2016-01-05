<div class="panel panel-info" style="margin-top: 30px;">
    <div class="panel-heading">
          <div class="panel-title">Register</div>
    </div>
    <div class="panel-body">
        <div>
            <dfForm action="/register" role="form">
            <dfIfChildErrors ref="">
            <div class="alert alert-dismissable alert-danger">
            <dfChildErrorList ref="" />
            </div>
            </dfIfChildErrors>
            <div class="form-group">
                 <dfLabel ref="name">Username:</dfLabel>
                 <dfInputText class="form-control" ref="name"/>

                 <dfLabel ref="email">Email:</dfLabel>
                 <dfInputText class="form-control" ref="email"/>

                 <dfLabel ref="password1">Password</dfLabel>
                 <dfInputPassword class="form-control" ref="password1"/>

                 <dfLabel ref="password2">Repeat password</dfLabel>
                 <dfInputPassword class="form-control" ref="password2"/>
                 <br>
                 <dfInputSubmit class="btn btn-lg btn-success btn-block" value="submit"/>
            </div>
            </dfForm>
        </div>
    </div>
</div>
