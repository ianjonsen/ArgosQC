# ArgosQC 0.10-2.9001

* Kinéis locations: the `aniMotum` error ellipse scaling parameter `psi` is now fixed in the first-pass SSM fit for tags with Kinéis locations, in both near real-time and delayed-mode QC. Kinéis errors are serially correlated through CLS Kalman smoothing, and estimating `psi` collapses it towards zero and makes the SSM standard errors severely overconfident. Tags without Kinéis locations are fitted as before, with `psi` estimated.

* Kinéis tags are identified from the data (`qc_kineis_tags()`): for SMRU tags, every error ellipse is a circle (semi-major axis equal to semi-minor axis, orientation 0), which is how SMRU encodes the Kinéis error radius; for Wildlife Computers tags, any location with `Type` "Kineis" in the Locations file.

* New optional `fix.psi` entry in the config `model` block (true or false) overrides the Kinéis detection for all tags.

* `multi_filter()` gains a `fix.psi` argument: a logical vector with one element per track.

# ArgosQC 0.9-1

* Wildlife Computers QC workflow can now download tag deployment metadata via WC API

# ArgosQC 0.8-5

* QC workflow details specified via .JSON config files

* simplified functions common to both tag data types


# ArgosQC 0.7-2

* handles both SMRU & WC tag data files

* single function calls to QC either SMRU or WC tag data

* vignettes added


# ArgosQC 0.5-10

* initial working build
