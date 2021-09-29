SELECT DISTINCT
	   m.ano_mes
	 , o.gr_contratacao contratacao
	 , o.gr_sgmt_assistencial segmentacao
	 , o.lg_odontologico in_odonto
	 , CASE o.obstetricia WHEN 'Com obstetrícia' THEN 1 ELSE 0 END in_obstetricia
	 , o.tipo_financiamento tipo
	 , o.abrangencia_cobertura abrangencia
	 , o.fator_moderador fator
	 , o.acomodacao_hospitalar acomodacao
	 , o.livre_escolha internacao
	 , v.cd_faixa_etaria
	 , c.nm_regiao
	 , CAST(replace(v.vl_coml_min,',','.') AS REAL) mensalidade
FROM rps_cadop  o 	INNER JOIN
	 nt_vc_mes  m 	ON o.id_plano = m.id_plano INNER JOIN
	 nt_geo_mun g	ON m.cd_nota = g.cd_nota INNER JOIN
	 geo_mun	c	ON g.cd_municipio = c.cd_municipio INNER JOIN
	 nt_vc 		v 	ON g.cd_nota = v.cd_nota
WHERE o.gr_contratacao <> "Coletivo empresarial";