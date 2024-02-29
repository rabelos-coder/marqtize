import { headers } from "next/headers";
import { getTranslations } from "next-intl/server";
import { Col, Container, Row } from "reactstrap";

import { LoginForm } from "@/components/auth/LoginForm";
import RatioImage from "@/components/backend/RatioImage";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("signIn"));

  return {
    title,
  };
}

export default async function LoginPage() {
  const header = headers();
  const host = header.get("host") ?? "";

  return (
    <Container fluid>
      <Row>
        <Col xl={7} className="b-center bg-size">
          <RatioImage
            className="bg-img-cover bg-center img-fluid w-100"
            src={`/assets/images/login/2.jpg`}
            alt=""
          />
        </Col>
        <Col xl={5} className="p-0">
          <LoginForm host={host} alignLogo="text-start" />
        </Col>
      </Row>
    </Container>
  );
}
