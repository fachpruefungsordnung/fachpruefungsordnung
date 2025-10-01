import clsx from "clsx";
import Heading from "@theme/Heading";
import styles from "./styles.module.css";

const FeatureList = [
  {
    title: "Team Collaboration",
    Svg: require("@site/static/img/undraw_docusaurus_mountain.svg").default,
    description: (
      <>
        Create projects, manage groups, and collaborate with university
        personnel.
      </>
    ),
  },
  {
    title: "Version Control",
    Svg: require("@site/static/img/undraw_docusaurus_tree.svg").default,
    description: (
      <>Track changes and maintain document history for all regulations.</>
    ),
  },
  {
    title: "Advanced Editing",
    Svg: require("@site/static/img/undraw_docusaurus_react.svg").default,
    description: (
      <>Write using a dedicated markup language with live preview.</>
    ),
  },
];

function Feature({ Svg, title, description }) {
  return (
    <div className={clsx("col col--4")}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
