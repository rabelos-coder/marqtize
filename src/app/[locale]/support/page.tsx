import { getTranslations } from "next-intl/server";
import { Container } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { concatTitle } from "@/utils/helpers";

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale, namespace: "translations" });
  const title = concatTitle(t("support"));

  return {
    title,
  };
}

export default function SupportPage() {
  return (
    <LandingLayout>
      <Header
        title="Support"
        description="orem ipsum dolor sit, amet consectetur adipisicing elit. Qui quisquam animi temporibus ipsum iusto necessitatibus laudantium beatae."
      />
      <section className="bg-white py-10">
        <Container className="container px-5">
          <h1>This is a basic content page.</h1>
          <p className="lead">
            You can use this page as a starting point to create your own custom
            pages, or choose an already built example page to start development!
          </p>
          <p>
            Lorem ipsum dolor sit, amet consectetur adipisicing elit. Qui
            quisquam animi temporibus ipsum iusto necessitatibus laudantium
            beatae. Eligendi dolorum laudantium numquam? Officiis nemo error
            animi aliquam dolor consequatur ducimus unde.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit. Qui
            repellat magni eaque beatae explicabo fugit placeat earum, dolores
            quaerat aperiam vero adipisci quidem minus officiis blanditiis unde?
            Incidunt, ea ad.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit.
            Perspiciatis sed illum soluta, quaerat et deleniti magnam
            laudantium, non omnis numquam quos placeat. Porro autem consectetur
            dolor minima voluptatum modi maiores.
          </p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
