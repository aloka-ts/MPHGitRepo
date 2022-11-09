package com.agnity.ph.ainscf.acg;

import java.io.File;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import com.agnity.ph.ainscf.acg.config.ApplicationRule;
import com.agnity.ph.ainscf.acg.config.OverlLoadRulesModel;
import com.agnity.ph.ainscf.acg.config.OverloadLevelRule;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

public class ACGAppSpecificRuleParser {

	private static ConcurrentHashMap<String, ACGAppSpecificRule> appRulesMap = new ConcurrentHashMap<String, ACGAppSpecificRule>();

	private static Logger logger = Logger
			.getLogger(ACGAppSpecificRuleParser.class);

	public static void main(String[] args) {

		System.out.println("parseRules-->");
		parseRules();
	}

	// root:
	// OverloadLevelRules:
	// Application:
	// SessionCountBase: 36000
	// AppId: 249
	// OverloadLevel:
	// -
	// SessionCountPmi: 40000
	// CPU: 65
	// Memory: 65
	// Level: 3.1
	// -
	// SessionCountPmi: 45000
	// CPU: 67
	// Memory: 67
	// Level: 3.2

	// Browser: FIREFOX
	// Module Name:
	// - ABC: Yes
	// - PQR: No

	public static void parseRules() {

		Yaml yaml = new Yaml(new Constructor(OverlLoadRulesModel.class));

		Reader yamlFile = null;
		String yamlpath = "C:\\Users\\reeta\\GitLab\\MPHGitRepo_nb\\src\\com\\agnity\\ph\\ainscf\\acg\\AcgAppRules.yml";
		// try {
		// String filepath = Constants.ASE_HOME + "//conf//AcgAppRules.yml";

		OverlLoadRulesModel rules = null;
		ObjectMapper mapper = new ObjectMapper(new YAMLFactory());
		try {
			rules = mapper.readValue(new File(yamlpath),
					OverlLoadRulesModel.class);
			System.out.println(rules);// ReflectionToStringBuilder.toString(rules,ToStringStyle.MULTI_LINE_STYLE));
		} catch (Exception e) {
			logger.error("Exception occured while parsing overload rules");

		}
		if (rules != null) {
			ArrayList<OverloadLevelRule> overLoadRules = rules
					.getOverloadLevelRules();

			Iterator<OverloadLevelRule> itr = overLoadRules.iterator();
			while (itr.hasNext()) {

				System.out.println("appRulesMap -->" + appRulesMap);

				OverloadLevelRule rule = itr.next();
				ApplicationRule appRule = rule.getApplicationRule();
				String appId = appRule.getAppId();
				ACGAppSpecificRule appSpecRule = new ACGAppSpecificRule();
				appSpecRule.setRule(appRule);
				appRulesMap.put(appId, appSpecRule);
			}

			System.out.println("appRulesMap -->" + appRulesMap);
		}
	}

	public static ACGAppSpecificRule getApplicationRule(String appId) {
		return appRulesMap.get(appId);
	}

	public static ConcurrentHashMap<String, ACGAppSpecificRule> getAllApplicationRules() {
		return appRulesMap;
	}
}
