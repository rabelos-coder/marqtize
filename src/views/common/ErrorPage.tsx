import Image from "next/image";
import { Col, Container } from "reactstrap";

import { Link } from "@/navigation";
import { CommonErrorPageProps } from "@/types/common";

export const ErrorPage = ({
  tittle,
  description,
  tittleClassName,
  BtnClassName,
}: CommonErrorPageProps) => {
  return (
    <div className="page-wrapper compact-wrapper" id="pageWrapper">
      <div className="error-wrapper">
        <Container>
          <Image
            width={100}
            height={100}
            className="img-100"
            src={`/assets/images/other-images/sad.png`}
            alt="Error"
          />
          <div className="error-heading">
            <h2 className={`headline ${tittleClassName}`}>{tittle}</h2>
          </div>
          <Col md={8} className="offset-md-2">
            <p className="sub-content">{description}</p>
          </Col>
          <div>
            <Link className={`btn  ${BtnClassName} btn-lg `} href={"/"}>
              BACK TO HOME PAGE
            </Link>
          </div>
        </Container>
      </div>
    </div>
  );
};
