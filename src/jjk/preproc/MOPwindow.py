#!/usr/bin/env python
""" Set up user menus to deal with inputs using curses"""


import curses
class MOPwindow:
    "Moving Object Pipeline window interface class"

    __list_window=None
    __help_window=None

    def __init__(self):
        self.__main=curses.initscr()
        curses.noecho()
        curses.cbreak()
        try:
            curses.start_color()
        except:
            pass
                    
        self.__main.keypad(1)
        self.__main.erase()
        self.__main.refresh()
        
    def help(self,help="This is the help you asked for?"):
        if not self.__help_window:
            self.__help_window=self.__main.subwin(35,0)
        _hw=self.__help_window
        _hw.erase()
        _hw.box()
        curses.beep()
        _hw.addstr(1,2,help)
        _hw.refresh()

    def end(self):        
        curses.nocbreak()
        self.__main.keypad(0)
        curses.echo()
        curses.endwin()

    def list(self,header,choices):
        "Display list of choices.  As many as we can get in a page."

        if not self.__list_window:
            (y,x)=self.__main.getmaxyx()
            self.__list_window = self.__main.subwin(35,x,0,0)
            
        _lw=self.__list_window


        _lw.keypad(1)
        (y_max,x_max)=_lw.getmaxyx()
        (y_0, x_0)=_lw.getbegyx()
        x_start=1+x_0
        _lw.box()
        ## Number of list items allowed.
        ### first entry in the list appears at page_top
        page_top=y_0+2
        ### the last entry display will be at page_bottom
        page_bottom = y_max-2

        ### break the list into chunks.
        max_items_per_page = page_bottom-page_top

        ### start at the top of the list
        top_item=0
        f=open('log.msg','w')
        first_item=page_top
        current_item=0
        item_list=[]
        while 1:
            _lw.erase()
            _lw.box()
            _lw.addstr(page_top-1,x_start,header)
            if top_item > len(choices):
                top_item=0
            for i in range(max_items_per_page):
                item=i+top_item
                if not item in range(len(choices)):
                    break
                _lw.addstr(i+page_top,x_start,choices[item])
            ### provide a hint that there is more info in the list
            ### setup where we are in the list
            last_item=item
            if top_item > 0 :
                _lw.addstr(page_bottom,x_start,"P(revious)")
            if last_item < len(choices):
                _lw.addstr(page_bottom,x_max-8,"N(ext)")
            while 1:
                c=_lw.getch(current_item-top_item+page_top,x_start)
                if c==curses.KEY_UP:
                    current_item=current_item-1
                elif c==curses.KEY_DOWN:
                    current_item=current_item+1
                elif c==ord(' '):
                    if current_item in item_list:
                        _lw.addstr(choices[current_item])
                        item_list.remove(current_item)
                    else:
                        _lw.addstr(choices[current_item],curses.A_REVERSE)
                        item_list.append(current_item)
                elif c==ord('P'):
                    top_item=top_item-max_items_per_page
                    current_item=top_item
                    break
                elif c==ord('N'):
                    top_item=top_item + max_items_per_page
                    current_item=top_item
                    break
                elif c==10:
                    return(item_list)
                elif c==ord('q'):
                    _lw.erase()
                    return(None)
		elif c==ord('x'):
		    choices[current_item]=choices[current_item][:4]+" "+choices[current_item][5:]
                    _lw.addstr(choices[current_item])
		else:
		    choices[current_item]=choices[current_item][:7]+chr(c).capitalize()+choices[current_item][8:]
                    _lw.addstr(choices[current_item])

                if current_item > last_item-1:
                    if last_item < len(choices):
                        top_item = top_item+1
                        break
                    else:
                        current_item=current_item-1
                if current_item < top_item :
                    if top_item > 0:
                        top_item = top_item-1
                        break
                    else:
                        current_item=current_item+1


