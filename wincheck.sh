export DISPLAY=:0.0
LOGIN=qwe@qwe
PASSWORD=qweqwe

curl -c /tmp/cookies --data "loginForm.name=$LOGIN&loginForm.password=$PASSWORD" -X POST -H "application/x-www-form-urlencoded"  "http://127.0.0.1:3000/login" 
WNDRAW=$(xdotool getactivewindow getwindowname)
WND=${WNDRAW//\/}
WND1=${WND//[[:space:]]}
echo $WND1
echo http://127.0.0.1:3000/checkwindow/"$WND1"
curl -b /tmp/cookies http://127.0.0.1:3000/checkwindow/"$WND1"

