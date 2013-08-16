__author__ = "David Rusk <drusk@uvic.ca>"

from ossos.gui.models.validation import ValidationModel


class TransAckValidationModel(ValidationModel):
    """
    This version of the UIModel requires confirmation that the view has
    been updated before it will allow further image transitions or
    accepting/rejecting.

    Any requests for further transitions while waiting for acknowledgement
    are simply ignored.
    """

    def __init__(self, workunit_provider,
                 image_manager,
                 synchronization_manager):
        super(TransAckValidationModel, self).__init__(workunit_provider,
                                                      image_manager,
                                                      synchronization_manager)
        self._source_transitioning = False
        self._observation_transitioning = False

    def is_waiting_for_source_transition(self):
        return self._source_transitioning

    def is_waiting_for_observation_transition(self):
        return self._observation_transitioning

    def is_waiting_for_transition(self):
        return (self.is_waiting_for_source_transition() or
                self.is_waiting_for_observation_transition())

    def expect_source_transition(self):
        if self.is_waiting_for_transition():
            # Don't allow additional events to be generated
            return

        self._source_transitioning = True
        self.expect_image_transition()

    def expect_observation_transition(self):
        if self.is_waiting_for_transition():
            # Don't allow additional events to be generated
            return

        self._observation_transitioning = True
        self.expect_image_transition()

    def acknowledge_image_displayed(self):
        self._source_transitioning = False
        self._observation_transitioning = False

    def next_obs(self):
        if self.is_waiting_for_transition():
            return

        super(TransAckValidationModel, self).next_obs()

    def previous_obs(self):
        if self.is_waiting_for_transition():
            return

        super(TransAckValidationModel, self).previous_obs()

    def next_source(self):
        if self.is_waiting_for_transition():
            return

        super(TransAckValidationModel, self).next_source()

    def previous_source(self):
        if self.is_waiting_for_transition():
            return

        super(TransAckValidationModel, self).previous_source()

    def next_item(self):
        if self.is_waiting_for_transition():
            return

        super(TransAckValidationModel, self).next_item()

    def accept_current_item(self):
        if self._should_disable_validation():
            return

        super(TransAckValidationModel, self).accept_current_item()

    def reject_current_item(self):
        if self._should_disable_validation():
            return

        super(TransAckValidationModel, self).reject_current_item()

    def _should_disable_validation(self):
        if self.is_waiting_for_transition() and self.is_processing_reals():
            return True

        if (self.is_waiting_for_source_transition() and
                self.is_processing_candidates()):
            return True

        return False
