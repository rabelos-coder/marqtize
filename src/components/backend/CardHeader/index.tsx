import { CardHeader as BootstrapCardHeader } from "reactstrap";

import { CommonCardHeadingPropsType } from "@/types/common";

export const CardHeader = ({
  headingClassName,
  smallHeading,
  Heading,
  span,
  span2,
  bigHeadingClassName,
}: CommonCardHeadingPropsType) => {
  return (
    <BootstrapCardHeader className={headingClassName ? headingClassName : ""}>
      {smallHeading ? (
        <h5>{smallHeading}</h5>
      ) : (
        <h4 className={bigHeadingClassName ? bigHeadingClassName : ""}>
          {Heading}
        </h4>
      )}
      {span && <span dangerouslySetInnerHTML={{ __html: span }} />}
      {span2 && <span dangerouslySetInnerHTML={{ __html: span2 }} />}
    </BootstrapCardHeader>
  );
};
