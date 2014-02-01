require(shiny)
require(igraph)
require(recharts)

options(shiny.transcode.json = FALSE)
ID = 'badbye'

load('badbye.RData')
ID.time = gsub('\\+0800', '', weibo$created_at)
ID.time = as.POSIXlt(ID.time, format='%a %b %d %H:%M:%S  %Y')

## repost vs original
reposts_time = weibo$retweeted_created_at
is_origin = is.na(reposts_time)

## followers' infomation, include name, gender and location
followers.info = sapply(followers$users, 
                        function(x) c(x$screen_name, x$gender, x$location))
followers.info = as.data.frame(t(followers.info), stringsAsFactors=F)
colnames(followers.info) = c('name', 'gender', 'location')

## final outputs
shinyServer(function(input, output){
  ##############################################################
  ################# histogram for weibo ########################
  ##############################################################
  output$time.hist = renderPlot({
    if (input$variable == 'months') {
      ### histogram by months
      hist(ID.time, "months", freq=T,
           col=rgb(0, 0, 0, 0.35), border=rgb(1, 1, 1, .6), 
           main="", xlab='', ylab='', format="%b %y")
      ## show repost VS original or not
      if (input$more) {
        hist(ID.time[is_origin], "months", freq=T, 
             col=rgb(0, 0, 0, 0.5), border=rgb(1, 1, 1, .6),
             axes=F, main="", xlab='', ylab='', format="%b %y", add=T)
        legend('topright', c('Repost', 'Original'), bty='n', 
               fill=c(col=rgb(0, 0, 0, 0.35),
                      col=rgb(0, 0, 0, 0.5)))
      }
    }
    else {
      ### histogram on a day
      hist(ID.time$h+0.2, breaks=24, axes=F,
           col=rgb(0, 0, 0, 0.35), border=rgb(1, 1, 1, .6),
           main="", xlab='', ylab='')
      label.time = c(0, 4, 8, 12, 16, 20, 24)
      axis(1, at=label.time, labels=paste0(label.time, ':00'), 
           col=rgb(0,0,0,0.3))
      axis(1, at=0:24, labels=F, col=rgb(0,0,0,0.3))
      n = max(table(ID.time$h))
      axis(2, at=pretty(0:n), labels = pretty(0:n), col=rgb(0,0,0,0.3))
      ## show repost VS original or not
      if (input$more){
        hist(ID.time$h[is_origin]+0.2, breaks=24, axes=F,
             col=rgb(0, 0, 0, 0.5), border=rgb(1, 1, 1, .6),
             main="", xlab='', ylab='', add=T)
        legend('topright', c('Repost', 'Original'), bty='n', 
               fill=c(col=rgb(0, 0, 0, 0.35),
                      col=rgb(0, 0, 0, 0.5))) 
      }
    }
  })
  ##############################################################
  ################# network graph of followers #################
  ##############################################################
  output$network = renderEcharts({
    recharts.init()
    network.node = data.frame(category = followers.info$gender, value = colSums(adjacency))
    rownames(network.node) = rownames(adjacency)
    eForce(adjacency, 
           propertyDf = network.node,
           opt = list(title = list(text = paste0(ID, '的社交网'), x = 'right', y ='bottom'),
                           series = list(minRadius = 8, maxRadius = 15, attractiveness = 0.8))
         )
  })
  ##############################################################
  ################# location of map visualization ##############
  ##############################################################
  output$map = renderEcharts({
    recharts.init()
    province = strsplit(followers.info$location, ' ')
    map.location = sapply(province, '[', 1)
    destination = c('广东', '青海', '四川', '海南', '陕西', 
                    '甘肃', '云南', '湖南', '湖北', '黑龙江',
                    '贵州', '山东', '江西', '河南', '河北',
                    '山西', '安徽', '福建', '浙江', '江苏', 
                    '吉林', '辽宁', '台湾',
                    ## 5个自治区
                    '新疆', '广西', '宁夏', '内蒙古', '西藏', 
                    ## 4个直辖市
                    '北京', '天津', '上海', '重庆',
                    ## 2个特别行政区
                    '香港', '澳门')
    china.index = map.location %in% destination  ## provinces in China
    location.table = table(map.location[china.index])  ## count each province
    map.data = data.frame(关注人数 = as.numeric(location.table))
    rownames(map.data) = names(location.table)
    eMap(map.data, opt = list(dataRange = list(min = 0, max = max(location.table), precision=1),
                              title = list(text = '关注的人在哪?', x='center', y='top'),
                              legend= list(data='')
                              ))

  })
  ##############################################################
  ######################### Something more #####################
  ##############################################################
  output$guess = renderPrint({
    cat('Users you usually @ or repost:', '\n')
    print(sort(sapply(followers.info$name, 
                function(x) length(grep(x, weibo$text))), 
         decreasing=T)[1:10])
    
    ## get friends' friends
    friends.friends = sapply(friends.info, 
                             function(x) sapply(x$users, function(y) y$name))
    cat('Users recommended for you:', '\n')
    friends.good = sort(table(unlist(friends.friends)), decreasing=T)
    print(friends.good[names(friends.good) %in% c(followers.info$name, ID) == F][1:10])
    
    ## users follow you
    followers.fen = followers.info$name[sapply(followers$users, function(x) x$follow_me)]
    ## a group of users you may be familiar with
    as.character(tags$h2("Maybe these are your peers? \n"))
    fen.matrix = adjacency[followers.fen, followers.fen]
    fen.graph = graph.adjacency(fen.matrix, 'directed')
    fen.graph$name = followers.fen
    fen.membership = clusters(fen.graph)$membership
    max.member = names(sort(table(fen.membership)))[no.clusters(fen.graph)]
    followers.fen[fen.membership==max.member]
  })
})