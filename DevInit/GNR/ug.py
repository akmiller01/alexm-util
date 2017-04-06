from reportlab.pdfgen import canvas
import xml.etree.ElementTree
import pdb


height = 1262
width = 892

canvas = canvas.Canvas("ug.pdf", pagesize=(width,height),bottomup=1)

e = xml.etree.ElementTree.parse("130055.xml").getroot()
fonts = {}
for page in e.findall("page"):
    for fontspec in page.findall("fontspec"):
        font = {}
        font["size"] = int(fontspec.get("size"))
        font["color"] = fontspec.get("color")
        fonts[fontspec.get("id")] = font
    for image in page.findall("image"):
        canvas.drawImage(image.get("src"),0,height-(int(image.get("top"))+int(image.get("height"))),int(image.get("width")),int(image.get("height")))
    for text in page.findall("text"):
        if text.text is not None:
            fontId = text.get("font")
            canvas.setFillColor(fonts[fontId]["color"])
            canvas.setFont("Helvetica",fonts[fontId]["size"])
            canvas.drawString(int(text.get("left")),height-(int(text.get("top"))+int(text.get("height"))),text.text)
    canvas.showPage()
 
canvas.save()