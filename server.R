
server <- function(input, output, session) {
  show_modal_spinner(spin = "flower", color = "#112446",
                     text = "Data loading....please wait", session = shiny::getDefaultReactiveDomain()) # show the modal window
  
  data<-callModule(module = QueryInfo, id = "id_1")
 
  observe({
  if(!is.null(data$data)){
 remove_modal_spinner()}

   data_rename<-callModule(module = RenameCode, id = "rename",reactive(data$data))
  # data_subset<-callModule(module = SubsetRegion, id = "subset",reactive(data_rename$data))
    callModule(module = Summary,id="summary",reactive(data_rename$data))
    callModule(module = Data,id="data",reactive(data_rename$data))
    callModule(module = Country,id="country",reactive(data_rename$data))
   # if(!is.null(data_subset$data)){
  #  callModule(module = Resume,id="region",reactive(data_subset$data)) 
  #  }
 
  
})
}

#URL Test
#?pid=fao_iati_projects_dbquery_adv&layer=fao_iati_projects_dbquery_adv_layer&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&feature_geom=true&strategy=ogc_viewparams&par=aggregation_method:none_withgeom&srs=EPSG:4326&dsd=%5B%7B"name":"Countries","definition":"Countries","primitiveCode":"country_iso3_code","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Sectors","definition":"Sectors","primitiveCode":"sector_code","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Regions","definition":"Region","primitiveCode":"region","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Funder","definition":"Funder","primitiveCode":"participating_org_funding","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Status","definition":"Status","primitiveCode":"status","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Start%20Date","definition":"Date%20of%20Start","primitiveCode":"start_date","primitiveType":"xsd:date","columnType":"attribute","minOccurs":0,"maxOccurs":1,"uom":null,"uomLabel":null%7D,%7B"name":"End%20Date","definition":"Date%20of%20End","primitiveCode":"end_date","primitiveType":"xsd:date","columnType":"attribute","minOccurs":0,"maxOccurs":1,"uom":null,"uomLabel":null%7D,%7B"name":"Budget","definition":"Budget%20in%20USD","primitiveCode":"budget_usd","primitiveType":"xsd:decimal","columnType":"variable","minOccurs":0,"maxOccurs":null,"uom":"USD","uomLabel":%7B"length":0,"prevObject":%7B"0":%7B%7D,"length":1,"prevObject":%7B"0":%7B%7D,"context":%7B%7D,"length":1%7D,"context":%7B%7D%7D,"context":%7B%7D%7D%7D,%7B"name":"Number%20of%20Projects","definition":"Number%20of%20Projects","primitiveCode":"nb_projects","primitiveType":"xsd:decimal","columnType":"variable","minOccurs":0,"maxOccurs":1,"uom":null,"uomLabel":null%7D,%7B"name":"geometry","definition":null,"primitiveCode":"geometry","primitiveType":"gml:MultiPolygonPropertyType","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Aggregation%20method","definition":"Method%20of%20aggregation","primitiveCode":"aggregation_method","primitiveType":"xsd:string","columnType":"attribute","minOccurs":1,"maxOccurs":1,"uom":null,"uomLabel":null%7D%5D