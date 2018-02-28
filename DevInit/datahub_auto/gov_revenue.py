from imf_sdmx import *

#Total government revenue (NCU)
gov_rev = imfDF("GFSR","A..S1311B.XDC.W0_S1_G1","1990","2018")
gov_rev.to_csv("gov_rev.csv",index=False)

#Total government revenue (% GDP)
gov_rev_pgdp = imfDF("GFSR","A..S1311B.XDC_R_B1GQ.W0_S1_G1","1990","2018")
gov_rev_pgdp.to_csv("gov_rev_pgdp.csv",index=False)

#Grant revenue (NCU)
grant_rev = imfDF("GFSR","A..S1311B.XDC.W0_S1_G13","1990","2018")
grant_rev.to_csv("grant_rev.csv",index=False)

#Grant revenue (% GDP)
grant_rev_pgdp = imfDF("GFSR","A..S1311B.XDC_R_B1GQ.W0_S1_G13","1990","2018")
grant_rev_pgdp.to_csv("grant_rev_pgdp.csv",index=False)