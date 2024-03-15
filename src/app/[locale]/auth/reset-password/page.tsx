import { getTranslations } from "next-intl/server";
import { Col, Container, Row } from "reactstrap";

import { ResetPasswordForm } from "@/views/auth/ResetPasswordForm";
import RatioImage from "@/components/backend/RatioImage";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("resetPassword"));

  return {
    title,
  };
}

export default async function ResetPasswordPage() {
  return (
    <Container fluid>
      <Row>
        <Col xl={7} className="b-center bg-size">
          <RatioImage
            className="bg-img-cover bg-center img-fluid w-100"
            src={`/assets/images/login/1.jpg`}
            alt=""
          />
        </Col>
        <Col xl={5} className="p-0">
          <ResetPasswordForm alignLogo="text-start" />
        </Col>
      </Row>
    </Container>
  );
}
