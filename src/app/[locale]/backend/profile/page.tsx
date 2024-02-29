import { getTranslations } from "next-intl/server";
import { Card, CardBody, Col, Container, Row } from "reactstrap";

import { Breadcrumbs } from "@/components/backend/Breadcrumbs";
import { CardHeader } from "@/components/backend/CardHeader";
import { AclGuard } from "@/components/backend/Guards/AclGuard";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("profile"));

  return {
    title,
  };
}

export default function ProfilePage() {
  return (
    <AclGuard>
      <div className="page-body">
        <Breadcrumbs title="profile" pageTitle="profile" />
        <Container fluid>
          <Row>
            <Col sm="12">
              <Card>
                <CardHeader
                  smallHeading="Sample Card"
                  span="lorem ipsum dolor sit amet, consectetur adipisicing elit"
                />
                <CardBody>
                  <p>
                    Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed
                    do eiusmod tempor incididunt ut labore et dolore magna
                    aliqua. Ut enim ad minim veniam, quis nostrud exercitation
                    ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    Duis aute irure dolor in reprehenderit in voluptate velit
                    esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
                    occaecat cupidatat non proident, sunt in culpa qui officia
                    deserunt mollit anim id est laborum.
                  </p>
                </CardBody>
              </Card>
            </Col>
          </Row>
        </Container>
      </div>
    </AclGuard>
  );
}
