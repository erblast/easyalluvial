# compose palette from qualitative RColorBrewer palettes

uses c('#FF0065','#009850', '#A56F2B', '#005EAA', '#710500', '#7B5380',
'#9DD1D1') and then adds all unique values found in all qualitative
RColorBrewer palettes

## Usage

``` r
palette_qualitative()
```

## Value

vector with hex values

## See also

[`RColorBrewer`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

## Examples

``` r
palette_qualitative()
#>  [1] "#FF0065" "#009850" "#A56F2B" "#005EAA" "#710500" "#7B5380" "#9DD1D1"
#>  [8] "#7FC97F" "#BEAED4" "#FDC086" "#FFFF99" "#386CB0" "#F0027F" "#BF5B17"
#> [15] "#666666" "#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E" "#E6AB02"
#> [22] "#A6761D" "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C"
#> [29] "#FDBF6F" "#FF7F00" "#CAB2D6" "#6A3D9A" "#B15928" "#FBB4AE" "#B3CDE3"
#> [36] "#CCEBC5" "#DECBE4" "#FED9A6" "#FFFFCC" "#E5D8BD" "#FDDAEC" "#F2F2F2"
#> [43] "#B3E2CD" "#FDCDAC" "#CBD5E8" "#F4CAE4" "#E6F5C9" "#FFF2AE" "#F1E2CC"
#> [50] "#CCCCCC" "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FFFF33" "#A65628"
#> [57] "#F781BF" "#999999" "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854"
#> [64] "#FFD92F" "#E5C494" "#B3B3B3" "#8DD3C7" "#FFFFB3" "#BEBADA" "#FB8072"
#> [71] "#80B1D3" "#FDB462" "#B3DE69" "#FCCDE5" "#D9D9D9" "#BC80BD" "#FFED6F"
```
