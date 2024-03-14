import { getTranslations } from "next-intl/server";
import { FiArrowRight } from "react-icons/fi";
import { Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("aboutUs"));

  return {
    title,
  };
}

export default function BasicPage() {
  return (
    <LandingLayout>
      <Header
        title="Sobre Nós"
        description="Create beautiful pages with easy to edit content"
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={10}>
              <h2 className="mb-4">A quick guide to our company culture</h2>
              <p>
                Lorem ipsum dolor, sit amet consectetur adipisicing elit. Dolor,
                eos quisquam expedita quo officiis porro provident laborum.
                Earum, consequatur provident, ipsam at excepturi rerum laborum
                aliquam facere molestias mollitia recusandae.
              </p>
              <p>
                Lorem ipsum dolor sit amet consectetur adipisicing elit. Alias,
                ipsam? Perspiciatis sunt voluptatum officia non harum, dolores
                omnis fugiat nam ad optio cumque molestiae impedit dignissimos
                velit commodi aliquid iure?
              </p>
              <p className="mb-0">
                Lorem ipsum dolor sit amet consectetur, adipisicing elit.
                Exercitationem sapiente natus architecto aut porro! Vitae iusto
                praesentium recusandae debitis, cumque illum amet suscipit rem
                pariatur, magni iure laborum inventore in!
              </p>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                This is what we do
              </h4>
              <p>
                Lorem ipsum dolor, sit amet consectetur adipisicing elit. Dolor,
                eos quisquam expedita quo officiis porro provident laborum.
                Earum, consequatur provident, ipsam at excepturi rerum laborum
                aliquam facere molestias mollitia recusandae.
              </p>
              <p>
                Lorem ipsum dolor sit amet consectetur adipisicing elit. Alias,
                ipsam? Perspiciatis sunt voluptatum officia non harum, dolores
                omnis fugiat nam ad optio cumque molestiae impedit dignissimos
                velit commodi aliquid iure?
              </p>
              <p>
                Lorem ipsum dolor sit amet consectetur, adipisicing elit.
                Exercitationem sapiente natus architecto aut porro! Vitae iusto
                praesentium recusandae debitis, cumque illum amet suscipit rem
                pariatur, magni iure laborum inventore in!
              </p>
              <div className="card bg-light shadow-none">
                <div className="card-body">
                  <h6>Questions you should ask yourself</h6>
                  <ul className="mb-0">
                    <li className="text-italic">
                      Lorem ipsum dolor sit amet consectetur adipisicing elit.
                      Sed soluta fugiat eveniet, dignissimos facere quisquam,
                      odit suscipit aliquid magnam,?
                    </li>
                  </ul>
                </div>
              </div>
              <hr className="my-5" />
              <h4 className="mb-4">
                <div className="icon-stack bg-primary text-white me-2">
                  <FiArrowRight width={24} height={24} />
                </div>
                Move with urgency and focus
              </h4>
              <p className="lead">
                Lorem ipsum dolor sit amet consectetur adipisicing elit. Fugit
                dolorem suscipit veniam excepturi soluta ducimus tempora quasi,
                itaque odit libero, eos in dolore natus, ipsam consectetur
                voluptate nulla provident aliquam?
              </p>
              <p>
                Lorem ipsum dolor sit amet consectetur adipisicing elit. Cumque
                laboriosam consequuntur iusto vero consequatur beatae, tempore
                voluptatem rerum sit facilis aut ea optio eaque quaerat, libero
                recusandae soluta molestiae veniam.
              </p>
              <p>
                Lorem ipsum dolor sit amet consectetur adipisicing elit. Quae,
                ut! Ut ipsum debitis numquam. Perferendis, laudantium! Itaque ab
                velit eaque magnam quis quam libero saepe nobis culpa. Vitae,
                dignissimos iusto.
              </p>
              <div className="card bg-light shadow-none">
                <div className="card-body">
                  <h6>Questions you should ask yourself</h6>
                  <ul className="mb-0">
                    <li className="text-italic">
                      Lorem ipsum dolor sit amet, consectetur adipisicing elit?
                    </li>
                    <li className="text-italic">
                      Lorem ipsum dolor sit amet consectetur, adipisicing elit.
                      Quas nobis illum nam?
                    </li>
                  </ul>
                </div>
              </div>
              <hr className="my-5" />
              <h4>Feedback</h4>
              <p className="lead">
                In the spirit of thinking rigorously, we’d love your open and
                honest feedback on this guide.
                <a href="#!">Let us know what you think</a>.
              </p>
              <p>
                Lorem ipsum dolor, sit amet consectetur adipisicing elit. Harum
                culpa optio nihil id distinctio excepturi dignissimos, iure
                totam minima, natus ducimus.
              </p>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
