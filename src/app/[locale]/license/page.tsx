import { getTranslations } from "next-intl/server";
import { Col, Container, List, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import {
  ADDRESS,
  APP_META_TITLE,
  CITY,
  CNPJ,
  LICENSE_DATE,
  POSTCODE,
  STATE,
  STATE_UF,
} from "@/environment";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("license"));

  return {
    title,
  };
}

export default async function LicensePage({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "license" });

  return (
    <LandingLayout>
      <Header title={t("title")} description={t("description")} />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="mb-5">
            <Col>
              <p className="mb-3 text-justify">
                {t("paragraph1", {
                  company: APP_META_TITLE,
                  companyNumber: CNPJ,
                  companyCity: CITY,
                  companyState: STATE,
                  companyStateAbbr: STATE_UF,
                  companyAddress: ADDRESS,
                  companyPostcode: POSTCODE,
                })}
              </p>
              <p className="mb-4 text-justify">
                {t("paragraph2", { company: APP_META_TITLE })}
              </p>
              <p className="my-3 text-justify">
                <strong>1. {t("paragraph3")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>1.1 </strong>
                  {t.rich("paragraph3_1", {
                    company: APP_META_TITLE,
                    b: (chunk) => <strong>{chunk}</strong>,
                  })}
                </li>
                <li className="ms-4 m-2">
                  <strong>1.2 </strong>
                  {t.rich("paragraph3_2", {
                    company: APP_META_TITLE,
                    b: (chunk) => <strong>{chunk}</strong>,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>2. {t("paragraph4")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>2.1 </strong>
                  {t("paragraph4_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
                <li className="ms-4 m-2">
                  <strong>2.2 </strong>
                  {t("paragraph4_2", {
                    company: APP_META_TITLE,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>3. {t("paragraph5")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>3.1 </strong>
                  {t("paragraph5_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
                <li className="ms-4 m-2">
                  <strong>3.2 </strong>
                  {t("paragraph5_2")}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>4. {t("paragraph6")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>4.1 </strong>
                  {t("paragraph6_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>5. {t("paragraph7")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>5.1 </strong>
                  {t("paragraph7_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>6. {t("paragraph8")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>6.1 </strong>
                  {t("paragraph8_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                <strong>7. {t("paragraph9")}:</strong>
              </p>
              <List type="unstyled" className="mb-4 text-justify">
                <li className="ms-4 mb-2">
                  <strong>7.1 </strong>
                  {t("paragraph9_1", {
                    company: APP_META_TITLE,
                  })}
                </li>
              </List>
              <p className="my-3 text-justify">
                {t("paragraph10", { company: APP_META_TITLE })}
              </p>
              <p className="my-3 text-justify">
                {t("paragraph11", { date: LICENSE_DATE })}
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
