<div class="panel panel-info" style="margin-top: 30px;">
    <div class="panel-heading">
          <div class="panel-title">Add task</div>
    </div>
    <div class="panel-body">
        <div>
            <dfForm action="/register" role="form">
            <div class="form-group">
                 <dfLabel ref="name">Title:
                 <dfInputText class="form-control" ref="name" />
                 </dfLabel>

                 <dfLabel class="checkbox" ref="active"> Start now?
                 <dfInputCheckbox ref="active" />
                 </dfLabel>
                 <br>

                 <dfLabel ref="allowed">Semicolumnt(;)-separated list of keywords, that must be in opened windows (empty for no restrictions):
                 <dfInputText class="form-control"  ref="allowed" />
                 </dfLabel>

                 <br>
                 <dfInputSubmit class="btn btn-lg btn-success btn-block" value="submit"/>
            </div>
            </dfForm>
        </div>
    </div>
</div>
