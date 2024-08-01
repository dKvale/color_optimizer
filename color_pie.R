deg2rad <- function(x, ...) {
  ## Convert degrees to radians, using pi/2 as 90 degrees
  x * (pi / 180);
}

setTextContrastColor <- function(color,
 hclCutoff=60,
 rgbCutoff=127,
 colorModel=c("hcl", "rgb"),
 useGrey=0,
 keepAlpha=FALSE,
 alphaLens=0,
 bg=NULL,
 ...)
{
  ## Purpose is to provide a good contrasting text color, given a background color
  ## useGrey=TRUE will use slightly off-white and off-black in order to allow some
  ## visual contrast when labels slightly overlap the opposite color.
  ##
  ## useGrey may also be an integer between 0 and 50, defining how much the grey
  ## maxima differ from perfect black and white, as defined by a range of 0 to 100.
  ## For example, useGrey=20 will define the values to be grey20 through grey80.
  ##
  ## Finally, useGrey may be supplied with two values, indicating divergence from
  ## black and white, respectively.
  ##
  ## keepAlpha=TRUE will keep the original alpha value
  colorModel <- match.arg(colorModel);

  ## Apply the logic to useGrey
  useGrey <- rep(useGrey, length.out=2);
  useGrey[isTRUE(useGrey)] <- 15;
  useGrey[useGrey %in% c(FALSE,0)] <- 0;
  useGrey[useGrey > 100] <- 100;

  greyVals <- abs(c(0,-100) + useGrey);
  bwColors <- rgb2col(col2rgb(paste0("grey", greyVals)));

  if (length(bg) == 0) {
    if (length(dev.list()) > 0) {
      bg <- par("bg");
    } else {
      bg <- "white";
    }
  }

  if (colorModel %in% "rgb") {
    colRgbMean <- colMeans(col2rgb(color));
    if (any(col2alpha(unique(color)) < 1)) {
      ## If any color is transparent, use weighted mean with the
      ## background color
      colWeight <- col2alpha(color);
      colRgbBg <- colMeans(col2rgb(bg));
      colRgbMean <- (colRgbMean * colWeight + colRgbBg * (1 - colWeight));
    }
    iColor <- ifelse(colRgbMean > rgbCutoff,
                     bwColors[1],
                     bwColors[2]);
  } else {
    colL <- col2hcl(color)["L",];
    if (any(col2alpha(unique(color)) < 1)) {
      bgL <- col2hcl(bg)["L",];
      colWeight <- col2alpha(color);
      warpWeight <- warpAroundZero(1-colWeight, xCeiling=1, lens=alphaLens);
      colL <- ((colL) * (1-warpWeight) + (bgL) * warpWeight);
    }
    iColor <- ifelse(colL > hclCutoff,
                     bwColors[1],
                     bwColors[2]);
  }
  if (keepAlpha) {
    iColor <- alpha2col(x=iColor, alpha=col2alpha(color));
  }
  iColor;
}


shadowText <- function(x,
 y=NULL,
 labels=NULL,
 col="white",
 bg=setTextContrastColor(col),
 r=getOption("jam.shadow.r", 0.15),
 offset=c(0.15, -0.15),
 n=getOption("jam.shadow.n", 8),
 outline=getOption("jam.outline", TRUE),
 alphaOutline=getOption("jam.alphaOutline", 0.4),
 shadow=getOption("jam.shadow", FALSE),
 shadowColor=getOption("jam.shadowColor", "black"),
 alphaShadow=getOption("jam.alphaShadow", 0.2),
 shadowOrder=c("each", "all"),
 cex=par("cex"),
 font=par("font"),
 doTest=FALSE,
 ...)
{
  ## Purpose is to draw text with a border around it to help
  ## make text more visible even with light and dark features
  ## beneath the text.
  ## It can be used by overriding the text function prior to
  ## running plot functions, e.g.
  ## 'text<-shadowText'.
  ## Then reset to the default text function afterwards, e.g.
  ## 'text<-graphics::text'
  ##
  ## doTest=TRUE will display an example

  #cex <- rep(cex, length.out=length(labels));
  #font <- rep(font, length.out=length(labels));
  #srt <- rep(srt, length.out=length(labels));
  shadowOrder <- match.arg(shadowOrder);

  if (length(alphaOutline) == 0) {
    alphaOutline <- 0.7;
    options("jam.alphaOutline"=0.7);
  }
  if (doTest) {
    ## Example shadow text
    nullPlot(xlim=c(1,9),
             ylim=c(0,10),
             doBoxes=FALSE,
             doUsrBox=TRUE,
             ...);
    if (length(col) == 1 && col == "white") {
      col <- c("white", "white", "yellow", "green4", "red4", "blue");
      bg <- setTextContrastColor(col);
    }
    if (length(labels) == 0) {
      labels <- LETTERS[1:12];
    } else {
      labels <- rep(labels, length.out=12);
    }
    st1 <- shadowText(x=rep(2,9), y=9:1,
                      labels=c("outline=FALSE","shadow=FALSE",labels[1:7]),
                      outline=FALSE, shadow=FALSE,
                      col=col,
                      bg=bg,
                      cex=c(1.1, 1, 1, 1, 1, 1, 1, 1, 1),
                      offset=offset, n=n, r=r,
                      doTest=FALSE);
    st2 <- shadowText(x=rep(4,9), y=9:1-0.3,
                      labels=c("outline=TRUE","shadow=FALSE",labels[1:7]),
                      outline=TRUE, shadow=FALSE,
                      col=col,
                      bg=bg,
                      cex=c(1.1, 1, 1, 1, 1, 1, 1, 1, 1),
                      offset=offset, n=n, r=r,
                      doTest=FALSE);
    st3 <- shadowText(x=rep(6,9), y=9:1,
                      labels=c("outline=FALSE","shadow=TRUE",labels[1:7]),
                      outline=FALSE, shadow=TRUE,
                      col=col,
                      bg=bg,
                      cex=c(1, 1.1, 1, 1, 1, 1, 1, 1, 1),
                      offset=offset, n=n, r=r,
                      doTest=FALSE);
    st4 <- shadowText(x=rep(8,9), y=9:1-0.3,
                      labels=c("outline=TRUE","shadow=TRUE",labels[1:7]),
                      outline=TRUE, shadow=TRUE,
                      col=col,
                      bg=bg,
                      cex=c(1.1, 1.1, 1, 1, 1, 1, 1, 1, 1),
                      offset=offset, n=n, r=r,
                      doTest=FALSE);
    return(invisible(list(st1=st1, st2=st2, st3=st3)));
  }

  cex <- rep(cex, length.out=length(labels));
  font <- rep(font, length.out=length(labels));
  bg <- rep(bg, length.out=length(labels));
  xy <- xy.coords(x, y);
  xo <- r * strwidth("A");
  yo <- r * strheight("A");
  if (length(offset) == 0) {
    offset <- c(0.15, 0.15);
  }
  offset <- rep(offset, length.out=2);
  offsetX <- offset[1] * strwidth("A");
  offsetY <- offset[2] * strheight("A");

  ## Angular sequence with n steps
  theta <- tail(seq(0, 2*pi, length.out=n+1), -1);

  ## Outline has no offset
  if (outline) {
    ## Make a matrix of coordinates per label
    outlineX <- matrix(ncol=n,
                       byrow=TRUE,
                       rep(xy$x, each=n) + cos(theta) * xo);
    outlineY <- matrix(ncol=n,
                       byrow=TRUE,
                       rep(xy$y, each=n) + sin(theta) * yo);
    outlineLabels <- matrix(ncol=n,
                            byrow=TRUE,
                            rep(labels, each=n));
    outlineColors <- matrix(ncol=n,
                            byrow=TRUE,
                            nrow=length(labels),
                            rep(alpha2col(bg, alpha=alphaOutline), each=n));
  } else {
    outlineX <- outlineY <- outlineLabels <- outlineColors <- NULL;
  }

  ## Shadow has offset
  if (shadow) {
    ## Make a matrix of coordinates per label
    shadowX <- matrix(ncol=n, byrow=TRUE,
                      rep(xy$x + offsetX, each=n) + cos(theta)*xo*1.5);
    shadowY <- matrix(ncol=n, byrow=TRUE,
                      rep(xy$y + offsetY, each=n) + sin(theta)*yo*1.5);
    shadowLabels <- matrix(ncol=n, byrow=TRUE,
                           rep(labels, each=n));
    shadowColors <- matrix(ncol=n, nrow=length(labels), byrow=TRUE,
                           rep(alpha2col(shadowColor, alpha=alphaShadow), each=n));
  } else {
    shadowX <- shadowY <- shadowLabels <- shadowColors <- NULL;
  }

  ## Append label coordinates to shadow coordinates so the shadows
  ## are drawn first, for each label in order. This order ensures
  ## that overlaps are respected without any labels appearing above
  ## another label shadow out of order.
  allX <- cbind(shadowX, outlineX, xy$x);
  allY <- cbind(shadowY, outlineY, xy$y);
  allColors <- cbind(shadowColors, outlineColors,
                     rep(col, length.out=length(labels)));
  allLabels <- cbind(shadowLabels, outlineLabels, labels);
  if ("each" %in% shadowOrder) {
    allX <- t(allX);
    allY <- t(allY);
    allColors <- t(allColors);
    allLabels <- t(allLabels);
    cex <- rep(cex, each=nrow(allX));
    font <- rep(font, each=nrow(allX));
  }
  #allCex <- rep(cex, n+1);
  #allFont <- rep(font, n+1);
  #allSrt <- rep(srt, n+1);

  ## Draw labels with one text() call to make it vectorized
  graphics::text(x=c(allX),
                 y=c(allY),
                 labels=c(allLabels),
                 col=c(allColors),
                 cex=cex,
                 font=font,
                 ...);
  return(invisible(list(
    allX=allX,
    allY=allY,
    allColors=allColors,
    allLabels=allLabels,
    cex=cex,
    font=font)));
}



color_pie <- function(colors,
 border=colors,
 lwd=2,
 radius=1.1,
 label_radius=radius*0.65,
 add=FALSE,
 init.angle=NULL,
 clockwise=TRUE,
 ...)
{
  ##
  if (is.list(colors)) {
    radius_seq <- head(
      seq(from=radius,
          to=0.3,
          length.out=length(colors)+1),
      length(colors));
    if (!is.list(border)) {
      border <- as.list(border);
    }
    radius_diff <- head(c(diff(radius_seq)/2, -radius*0.25), 1);
    border <- rep(border,
                  length.out=length(colors));
    l <- lapply(seq_along(colors), function(i){
      if (i == 1) {
        color_pie(colors=colors[[i]],
                  border=border[[i]],
                  lwd=lwd,
                  add=(i > 1),
                  init.angle=init.angle,
                  clockwise=clockwise,
                  radius=radius_seq[i],
                  label_radius=radius_seq[i]*0.92 + radius_diff,
                  ...);
      } else {
        color_pie(colors=colors[[i]],
                  border=border[[i]],
                  lwd=lwd,
                  add=(i > 1),
                  init.angle=init.angle,
                  clockwise=clockwise,
                  radius=radius_seq[i],
                  label_radius=radius_seq[i]*0.92 + radius_diff);
      }
    });
    return(invisible(l));
  }
  if (length(init.angle) == 0) {
    if (clockwise) {
      init.angle <- 90 + 360 / length(colors) / 2;
    } else {
      init.angle <- 90 - 360 / length(colors) / 2;
    }
  }

  # op <- par(no.readonly=TRUE);
  if (length(colors) == 1) {
    lwd <- 0.001;
  }
  op <- par("xpd"=TRUE, "lwd"=lwd);
  on.exit(par(op));
  if (TRUE %in% add) {
    par("new"=TRUE);
  }

  pie(x=rep(1, length.out=length(colors)),
      col=colors,
      border=border,
      labels="",
      lwd=lwd,
      radius=radius,
      init.angle=init.angle,
      clockwise=clockwise,
      ...);
  if (length(names(colors)) > 0) {
    par("new"=TRUE);
    par("lwd"=0.001);
    label_angles <- round(head(seq(from=init.angle - 180 / length(colors),
                                   to=(init.angle - 360 - 180 / length(colors)),
                                   length.out=length(colors) + 1), -1)) %% 360;
    label_angles1 <- ((label_angles + 89) %% 180 - 89) %% 360;
    angle_switch <- (label_angles != label_angles1) * 1;
    lx <- cos(deg2rad(label_angles)) * label_radius;
    ly <- sin(deg2rad(label_angles)) * label_radius;
    for (k in split(seq_along(lx), paste0(label_angles, "_", angle_switch))) {
       shadowText(x=lx[k],
                  y=ly[k],
                  adj=c(head(angle_switch[k], 1), 0.5),
                  col=setTextContrastColor(colors[k], useGrey=15),
                  labels=names(colors)[k],
                  srt=head(label_angles1[k], 1))
    }
  }
  invisible(colors);
}
