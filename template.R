## -*- coding: utf-8 -*-

if(interactive())source('~/cs/code/r/tasks/task.R')

## Globalne parametry zadania

MAX.REACTION.TIME = 3000
FIXATION.TIME = 500
POST.FIXATION.TIME = 2000

## Globalne obiekty graficzne

TXT$set.string("Proszę nacisnąć spację")
center(TXT, WINDOW)
FX = fixation(WINDOW)
STIM = new(Text)
STIM$set.font(FONT)

## Funkcje pomocnicze, typu rysowanie bodźców

draw.stim = function(side){
    STIM$set.string(c(left = 'LEWO', right = 'PRAWO')[side])
    center.win(STIM)
    WINDOW$draw(STIM)
}

## Dwa klawisze w kluczu reakcyjnym

KEYS <<- c(Key.Left, Key.Right)

trial.code = function(trial, side = 'left'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            draw.scale.boxes()
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            draw.stim(side)
            WINDOW$display()
            stim.onset = CLOCK$time
            CORRECT.KEY <<- c(left = Key.Left, right = Key.Right)[side]
            ACC <<- RT <<- NULL
            state = 'measure-reaction'
        }, 'measure-reaction' = {
            if(!is.null(ACC) || ((CLOCK$time - stim.onset) > MAX.REACTION.TIME))state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            return(list(rt = ifelse(is.null(RT), MAX.REACTION.TIME, RT - stim.onset),
                        acc = ifelse(is.null(ACC), 2, ACC)))
        })
    }
}

TASK.NAME <<- 'template'
gui.show.instruction("Za chwilę pojawi się okno danych osobowych")
## gui.user.data()
USER.DATA = list(name = 'admin', age = 37, gender = 'M')
source.random.condition()
run.trials(trial.code, expand.grid(side = c('left', 'right')))
if(!interactive())quit("no")
