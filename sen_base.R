######################################################################
## Copyright (C) 2015, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

# This program generates a data visualization of U.S. senate campaign funding.

options("scipen" = 100, "digits" = 2)

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Pretty print routines - generic, money, and percentage versions.

pp <- function(val, digits = 2) {
    format(ifelse ( digits > 0, round(val, digits = digits), as.integer(val) ),
           nsmall = ifelse ( digits > 0, digits, 0 ),
           scientific = FALSE,
           big.mark = ',')
}
money <- function(val, digits = 2) { paste('$', pp(val, digits), sep = '') }
pct <- function(val, digits = 2) { paste(pp(val, digits), '%', sep = '') }

# Read .csv files downloaded from followthemoney.com, process lightly, and save as .RData for quick load.

fname <- "./Senate.RData"
if ( file.exists(fname) ) {
    load(fname)
} else {
    s1 <- read.csv("./Senate1.csv")
    s2 <- read.csv("./Senate2.csv")
    senate <- rbind(s1, s2)
    senate <- senate[, c("Contributor", "General_Industry", "Broad_Sector", "Amount", "Purpose",
                         "Candidate", "General_Party", "Election_Jurisdiction", "Election_Year")]
    colnames(senate) <- c("contributor", "industry", "sector", "amount", "purpose",
                          "candidate", "party", "state", "year")
    save(senate, file = fname)
}

# Identify (up to) two most recent winners for each state.

office.holders <- NULL
tmp <- as.data.frame(summarise(group_by(senate, state, candidate),
                                          year = max(year)))
for ( state in state.abb ) {
    candidates <- tmp[tmp$state == state,]
    candidates <- candidates[order(candidates$year, decreasing = TRUE),]
    cnt <- min(2, dim(candidates)[1])
    if ( is.null(office.holders) ) {
        office.holders <- candidates[1:cnt,]
    } else {
        office.holders <- rbind(office.holders, candidates[1:cnt,])
    }
}

# Function to generate a plot showing what percentage of contributors accounted for
# 25/50/75 percent of contributions respectively.  Input bycont is expected to be a
# data frame where each row summarises a contributor's donations to the campaign
# sorted in descending amount order.

plot.pct.contributions <- function(bycont) {
    
    # Construct i.pct.tot column which is running total of percentage of contributors.
    bycont$i <- seq(dim(bycont)[1])
    bycont$i.pct.tot <- 100 * (bycont$i / dim(bycont)[1])
    
    # Construct amt.pct.tot column which is running total of percentage of amounts contributed.
    bycont$amt.pct <- 100 * (bycont$amount / sum(bycont$amount))
    bycont$amt.pct.tot <- cumsum(bycont$amt.pct)
    
    # Interpolate to get percentage of contributors at contribution percentages of 25/50/75.
    # Pad data with (0, 0) at low end so interpolation has a starting point for small values.
    xylst <- approx(x = c(0.0, bycont$amt.pct.tot), y = c(0.0, bycont$i.pct.tot), n = 1000)
    breaks <- c(xylst$y[[250]], xylst$y[[500]], xylst$y[[750]])
    breaks.delta <- c(breaks[1], breaks[2] - breaks[1], breaks[3] - breaks[2], 100 - breaks[3])
    
    # Construct data frame for stacked bar chart.
    bardata <- data.frame(pct = c(breaks.delta, 25, 25, 25, 25),
                          what = c(rep("Donors", 4), rep("Dollars", 4)),
                          
                          section = rep(seq(4), 2))
    # Construct data frame for connecting line segents.
    segdata <- data.frame(Donors = breaks,
                          Dollars = c(25, 50, 75))
    
    # Factor on "what" column so Donors appear to left of Dollars.
    bardata$what <- factor(bardata$what, levels = c("Donors", "Dollars"))
    
    # Make and return the plot.
    ggplot() +
        geom_bar(data = bardata,
                 aes(x = what, y = pct, fill = section),
                 stat = "identity", colour = "black", width = 0.4) +
        scale_fill_gradientn(colours = heat.colors(5)) +
        geom_segment(data = segdata,
                     aes(x = 1.2, y = Donors, xend = 1.8, yend = Dollars)) +
        geom_text(data = segdata,
                  size = 3, fontface = "bold",
                  aes(x = 1, y = Donors, hjust = 0.5, vjust = -0.4,
                      label = sapply(Donors, pct, digits = 1))) +
        geom_text(data = segdata,
                  size = 3, fontface = "bold",
                  aes(x = 2, y = Dollars, hjust = 0.5, vjust = -0.4,
                      label = sapply(Dollars, pct, digits = 0))) +
        scale_y_continuous(breaks = c(25, 50, 75, 100)) +
        labs(y = "Percentage") +
        ggtitle("% Donors vs % Dollars") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(size = 10, face = "bold", color = "black"),
              axis.text.x = element_text(size = 10, face = "bold", color = "black"),
              axis.text.y = element_text(size = 10, face = "bold", color = "black"),
              legend.position = "none")
}

# Function to generate horizontal bar chart of contributions by sector.  Since sector names
# are long and take up too much horizontal space, we remove axis labels and overlay sector
# names (with dollar values prepended) on the bars themselves.
    
plot.sectors <- function(sectors) {
    
    # Turn sector column into factor such that y-axis is in decreasing amount order.
    sectors$sector <- factor(sectors$sector,
                             levels = sectors$sector[order(sectors$amount)])
    sectors$label <- paste(" ", sapply(sectors$amount, money, digits = 0),
                           "-", sectors$sector)
    
    # Make and return the plot.
    ggplot() +
        geom_bar(data = sectors,
                 aes(x = sector, y = amount),
                 position = "identity",
                 stat = "identity",
                 color = "pink",
                 fill = "pink") +
        coord_flip() +
        # Annotate with numeric labels
        geom_text(data = sectors,
                  size = 4,
                  fontface = "bold",
                  aes(x = sector,
                      y = 0,
                      label = label,
                      hjust = 0)) +
        ggtitle("Contributions by Sector") +
        theme_bw() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank())
}

# Function to expand aggregated unitemized donations into individual unique donations of
# maximum unreportable donation size as per Federal Election Commission guidelines for
# congressional races: http://www.fec.gov/pdf/candgui.pdf.

expand.unitemized <- function(totals) {
    
    # Split into itemized (id) and unitemized (ud) donations.
    id <- totals[totals$contributor != "UNITEMIZED DONATIONS",]
    ud <- totals[totals$contributor == "UNITEMIZED DONATIONS",]
    
    # Input should be summarized by amount so expecting 1 or 0 unitemized donations only.
    stopifnot(dim(ud)[1] <= 1)
    
    # Nothing to do if no unitemized donations or they total less than single maximum donation.
    # Max unitemized donation as per FEC is $200.
    if ( 0 == dim(ud)[1] || ud[1, "amount"] <= 200.0 ) {
        return(totals)
    }
    
    # Calculate how many at $200 and remainder.
    cnt.ud <- ud[1, "amount"] %/% 200
    remainder.ud <- ud[1, "amount"] %% 200
    
    # Construct data.frame of cnt.ud plus remainder.ud unique donations.
    ud.new <- data.frame(contributor = paste("UNITEMIZED DONOR -", as.character(seq(cnt.ud))),
                         amount = rep(200.0, cnt.ud))
    if ( remainder.ud > 0.0 ) {
        ud.new <- rbind(data.frame(contributor = "UNITEMIZED DONOR - 0",
                                   amount = remainder.ud),
                        ud.new)
    }
    
    retval <- rbind(id, ud.new)
    stopifnot(sum(totals$amount) == sum(retval$amount))
    retval
}

# Function to process a single state, eg: "AK", generating two plots and one listing.
# First plot shows which percentage of contributors gave 25/50/75% of campaign funds respectively.
# Second plot dhoes contributions by industry segment.
# Listing shows top 25 contributors to the campaign.

do.state <- function(state) {
    
    # Iterate over senators in desired state.
    candidates <- office.holders[office.holders$state == state,]

    for ( i in seq(dim(candidates)[1]) ) {
        
        candidate <- candidates[i, "candidate"]
        year <- candidates[i, "year"]
        contributions <- senate[senate$candidate == candidate & senate$year == year,]
        party <- contributions[1, "party"]
        
        # Summarise contributions by contributor.
        totals <- as.data.frame(summarise(group_by(contributions, contributor),
                                          amount = sum(amount)))
        
        # Contribution refunds and candidate loan repayments have negative amount values and should be
        # reconciled by the summarise(..., amount = sum(amount)) mechanism.  Occasionally there are unreconciled
        # negative values - either as a result of bad data, or more likely because a candidate loan from
        # a prior campaign was reimbursed from donations to the current campaign.  Empirical testing shows
        # these to typically be less than 0.5% of the total, so we remove them.
        totals <- totals[totals$amount > 0,]
        donations.total <- sum(totals$amount)
        
        # Replace single aggregated unitemized donation with N donations at the reporting limit.
        totals <- expand.unitemized(totals)
        
        # Order by decreasing contribution amount.
        totals <- totals[order(totals$amount, decreasing = TRUE),]
        
        # Generate data for contributions by industry sector plot.
        sectors <- as.data.frame(summarise(group_by(contributions, sector),
                                           amount = sum(amount)))
        
        # Generate plots and output.
        gg1 <- plot.pct.contributions(totals)
        gg2 <- plot.sectors(sectors)
        title <- paste(candidate, "-", party, "- Elected", year, "\n",
                       pp(dim(totals)[1], digits = 0), "donors gave",
                       money(donations.total, digits = 0), sep = ' ')
        grid.arrange(gg1, gg2, ncol = 2, top = textGrob(title, gp = gpar(fontface = "bold")))
        
        # Prettify totals data for top ten listing and print.
        totals$pct <- (totals$amount / sum(totals$amount)) * 100
        totals$pct <- sapply(totals$pct, pct, digits = 1)
        totals$amount <- sapply(totals$amount, money, digits = 0)
        colnames(totals) <- c("Contributor", "Amount", "Pct")
        print(head(totals, 25), row.names = FALSE)
    }
}

