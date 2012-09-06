procedure edhead (image)

string image {"", prompt="Image header to adjust"}
bool   verbose {"yes", prompt="Follow along?"}

begin
	string timage
	bool   tverbose
	timage = image
	
	hedit.show=verbose


imgets(timage//"[0]","edhead")

if (int(imgets.value)!=1) {

#hedit(timage//"[0]","edhead",1,add+,verify-)

#msccmd ("hedit $input rdnoise 5.0 add+ verify-",timage)
#msccmd ("hedit $input TRIMSEC [6:2049,5:4100] verify- add+",timage)
#msccmd ("hedit $input CCDSEC [1:2044,1:4096] verify- add+",timage)
#msccmd ("hedit $input BIASSEC [2054:2079,5:4100] verify- add+",timage)

msccmd ("hedit $input DATASEC [6:2049,5:4100] verify- add-", timage)

msccmd ("hedit $input DETSEC [6242:8285,8215:4120] verify- add-", timage, extname="chip09")

msccmd ("hedit $input DETSEC [4161:6204,8210:4115] verify- add-", timage, extname="chip08")

msccmd ("hedit $input DETSEC [2093:4136,8211:4116] verify- add-", timage, extname="chip07")

msccmd ("hedit $input DETSEC [2083:4126,1:4096] verify- add-", timage, extname="chip01")

msccmd ("hedit $input DETSEC [8315:10358,8210:4115] verify- add-", timage, extname="chip10" )

msccmd ("hedit $input DETSEC [2056:13,8216:4121] verify- add-", timage, extname="chip06" )

msccmd ("hedit $input DETSEC [1:2044,1:4096] verify- add-", timage, extname="chip00" )


msccmd ("hedit $input DETSEC [10395:12438,8203:4108] verify- add-", timage, extname="chip11")

}
end
