/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class AbilitiesAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  RangeSliderButton healthRegenerationSlider;

  private Checkbox openDoorCheckbox;
  private Checkbox closeDoorCheckbox;
  private Checkbox passDoorCheckbox;
  private boolean passDoorValue;
  private TextButton navigationTypeButton;
  private RangeSliderButton hoverHeightSlider;
  private RangeSliderButton minHoverHeightSlider;
  private RangeSliderButton swimDepthBelowSurfaceSlider;
  private RangeSliderButton swimHeightAboveFloorSlider;
  private NavigationType navigationType;

  public AbilitiesAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static String getNavigationTypeLabel(NavigationType navigationType) {
    return "navigation_type_" + navigationType.getAttributeName();
  }

  private static NavigationType getNextNavigationType(NavigationType navigationType) {
    NavigationType[] navigationTypes = NavigationType.values();
    return navigationTypes[(navigationType.ordinal() + 1) % navigationTypes.length];
  }

  private void refreshNavigationSliders() {
    NavigationDataCapable<?> navigationData = this.getEasyNPC().getEasyNPCNavigationData();
    boolean isFlying =
        this.navigationType == NavigationType.FLYING
            || (this.navigationType == NavigationType.DEFAULT && navigationData.canFly());
    this.hoverHeightSlider.visible = isFlying;
    this.minHoverHeightSlider.visible = isFlying;

    boolean isAquatic =
        this.navigationType == NavigationType.AQUATIC
            || (this.navigationType == NavigationType.DEFAULT && navigationData.canSwim());
    this.swimDepthBelowSurfaceSlider.visible = isAquatic;
    this.swimHeightAboveFloorSlider.visible = isAquatic;
  }

  private void refreshPassDoorCheckbox() {
    boolean impliedByDoorInteraction =
        this.openDoorCheckbox.selected() || this.closeDoorCheckbox.selected();
    this.passDoorCheckbox.active = !impliedByDoorInteraction;
    this.passDoorCheckbox.setSelected(impliedByDoorInteraction || this.passDoorValue);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.abilitiesAttributeButton.active = false;

    // Button rows
    int firstButtonRow = this.leftPos + 10;
    int secondButtonRow = this.leftPos + 100;
    int thirdButtonRow = this.leftPos + 200;

    // Attribute data
    AttributeDataCapable<?> attributeData = this.getEasyNPC().getEasyNPCAttributeData();
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();

    // Checkboxes
    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 25,
            EnvironmentalAttributeType.CAN_FLOAT.getAttributeName(),
            entityAttributes.getEnvironmentalAttributes().canFloat(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .environmentalAttributeChange(
                        this.getEasyNPCUUID(),
                        EnvironmentalAttributeType.CAN_FLOAT,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 25,
            EnvironmentalAttributeType.CAN_BREATHE_UNDERWATER.getAttributeName(),
            entityAttributes.getEnvironmentalAttributes().canBreatheUnderwater(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .environmentalAttributeChange(
                        this.getEasyNPCUUID(),
                        EnvironmentalAttributeType.CAN_BREATHE_UNDERWATER,
                        checkbox.selected())));

    this.openDoorCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                firstButtonRow,
                this.buttonTopPos + 45,
                MovementAttributeType.CAN_OPEN_DOOR.getAttributeName(),
                entityAttributes.getMovementAttributes().canOpenDoor(),
                checkbox -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .movementAttributeChange(
                          this.getEasyNPCUUID(),
                          MovementAttributeType.CAN_OPEN_DOOR,
                          checkbox.selected());
                  this.refreshPassDoorCheckbox();
                }));

    this.closeDoorCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                secondButtonRow,
                this.buttonTopPos + 45,
                MovementAttributeType.CAN_CLOSE_DOOR.getAttributeName(),
                entityAttributes.getMovementAttributes().canCloseDoor(),
                checkbox -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .movementAttributeChange(
                          this.getEasyNPCUUID(),
                          MovementAttributeType.CAN_CLOSE_DOOR,
                          checkbox.selected());
                  this.refreshPassDoorCheckbox();
                }));

    this.passDoorValue = entityAttributes.getMovementAttributes().canPassDoor();
    this.passDoorCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                thirdButtonRow,
                this.buttonTopPos + 45,
                MovementAttributeType.CAN_PASS_DOOR.getAttributeName(),
                this.passDoorValue,
                checkbox -> {
                  this.passDoorValue = checkbox.selected();
                  NetworkMessageHandlerManager.getServerHandler()
                      .movementAttributeChange(
                          this.getEasyNPCUUID(),
                          MovementAttributeType.CAN_PASS_DOOR,
                          checkbox.selected());
                }));
    this.refreshPassDoorCheckbox();
    this.addRenderableWidget(
        new HelpIcon(this.leftPos + 296, this.buttonTopPos + 47, "door_behavior.tooltip"));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 65,
            InteractionAttributeType.IS_PUSHABLE.getAttributeName(),
            entityAttributes.getInteractionAttributes().isPushable(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .interactionAttributeChange(
                        this.getEasyNPCUUID(),
                        InteractionAttributeType.IS_PUSHABLE,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 65,
            InteractionAttributeType.PUSH_ENTITIES.getAttributeName(),
            entityAttributes.getInteractionAttributes().pushEntities(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .interactionAttributeChange(
                        this.getEasyNPCUUID(),
                        InteractionAttributeType.PUSH_ENTITIES,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 85,
            InteractionAttributeType.CAN_BE_LEASHED.getAttributeName(),
            entityAttributes.getInteractionAttributes().canBeLeashed(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .interactionAttributeChange(
                        this.getEasyNPCUUID(),
                        InteractionAttributeType.CAN_BE_LEASHED,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 85,
            InteractionAttributeType.BLOCK_VEHICLE_MOUNTING.getAttributeName(),
            entityAttributes.getInteractionAttributes().blockVehicleMounting(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .interactionAttributeChange(
                        this.getEasyNPCUUID(),
                        InteractionAttributeType.BLOCK_VEHICLE_MOUNTING,
                        checkbox.selected())));
    this.addRenderableWidget(
        new HelpIcon(this.leftPos + 296, this.buttonTopPos + 87, "block_vehicle_mounting.tooltip"));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 105,
            MovementAttributeType.CAN_USE_NETHER_PORTAL.getAttributeName(),
            entityAttributes.getMovementAttributes().canUseNetherPortal(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .movementAttributeChange(
                        this.getEasyNPCUUID(),
                        MovementAttributeType.CAN_USE_NETHER_PORTAL,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 105,
            MovementAttributeType.IS_IMMOVABLE.getAttributeName(),
            entityAttributes.getMovementAttributes().isImmovable(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .movementAttributeChange(
                        this.getEasyNPCUUID(),
                        MovementAttributeType.IS_IMMOVABLE,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 125,
            EntityAttribute.SILENT.getAttributeName(),
            attributeData.getAttributeSilent(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityAttributeChange(
                        this.getEasyNPCUUID(), EntityAttribute.SILENT, checkbox.selected())));

    this.navigationType = entityAttributes.getMovementAttributes().navigationType();
    this.navigationTypeButton =
        this.addRenderableWidget(
            new TextButton(
                firstButtonRow + 135,
                this.buttonTopPos + 145,
                80,
                getNavigationTypeLabel(this.navigationType),
                onPress -> {
                  this.navigationType = getNextNavigationType(this.navigationType);
                  this.navigationTypeButton.setMessage(
                      TextComponent.getTextComponent(getNavigationTypeLabel(this.navigationType)));
                  NetworkMessageHandlerManager.getServerHandler()
                      .navigationTypeChange(this.getEasyNPCUUID(), this.navigationType);
                  this.refreshNavigationSliders();
                }));

    this.hoverHeightSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 135,
                this.buttonTopPos + 168,
                entityAttributes.getMovementAttributes().hoverHeight(),
                0.0D,
                16.0D,
                0.0D,
                0.5D,
                slider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .movementAttributeChange(
                            this.getEasyNPCUUID(),
                            MovementAttributeType.HOVER_HEIGHT,
                            slider.getTargetDoubleValue())));

    this.minHoverHeightSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 135,
                this.buttonTopPos + 189,
                entityAttributes.getMovementAttributes().minHoverHeight(),
                0.0D,
                16.0D,
                0.0D,
                0.5D,
                slider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .movementAttributeChange(
                            this.getEasyNPCUUID(),
                            MovementAttributeType.MIN_HOVER_HEIGHT,
                            slider.getTargetDoubleValue())));

    this.swimDepthBelowSurfaceSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 135,
                this.buttonTopPos + 168,
                entityAttributes.getMovementAttributes().swimDepthBelowSurface(),
                0.0D,
                32.0D,
                0.0D,
                0.5D,
                slider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .movementAttributeChange(
                            this.getEasyNPCUUID(),
                            MovementAttributeType.SWIM_DEPTH_BELOW_SURFACE,
                            slider.getTargetDoubleValue())));

    this.swimHeightAboveFloorSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 135,
                this.buttonTopPos + 189,
                entityAttributes.getMovementAttributes().swimHeightAboveFloor(),
                0.0D,
                16.0D,
                0.0D,
                0.5D,
                slider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .movementAttributeChange(
                            this.getEasyNPCUUID(),
                            MovementAttributeType.SWIM_HEIGHT_ABOVE_FLOOR,
                            slider.getTargetDoubleValue())));
    this.refreshNavigationSliders();

    this.healthRegenerationSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 135,
                this.buttonTopPos + 210,
                entityAttributes.getCombatAttributes().healthRegeneration(),
                0.0D,
                32.0D,
                0.0D,
                0.1D,
                slider ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .combatAttributeChange(
                            this.getEasyNPCUUID(),
                            CombatAttributeType.HEALTH_REGENERATION,
                            slider.getTargetDoubleValue())));
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    int sliderXOffset = -125;
    int sliderYOffset = 3;

    if (this.navigationTypeButton != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "navigation_type",
          this.navigationTypeButton.getX() + sliderXOffset,
          this.navigationTypeButton.getY() + sliderYOffset);
    }

    if (this.hoverHeightSlider != null && this.hoverHeightSlider.visible) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "hover_height",
          this.hoverHeightSlider.getX() + sliderXOffset,
          this.hoverHeightSlider.getY() + sliderYOffset);
    }

    if (this.minHoverHeightSlider != null && this.minHoverHeightSlider.visible) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "min_hover_height",
          this.minHoverHeightSlider.getX() + sliderXOffset,
          this.minHoverHeightSlider.getY() + sliderYOffset);
    }

    if (this.swimDepthBelowSurfaceSlider != null && this.swimDepthBelowSurfaceSlider.visible) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "swim_depth_below_surface",
          this.swimDepthBelowSurfaceSlider.getX() + sliderXOffset,
          this.swimDepthBelowSurfaceSlider.getY() + sliderYOffset);
    }

    if (this.swimHeightAboveFloorSlider != null && this.swimHeightAboveFloorSlider.visible) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "swim_height_above_floor",
          this.swimHeightAboveFloorSlider.getX() + sliderXOffset,
          this.swimHeightAboveFloorSlider.getY() + sliderYOffset);
    }

    if (this.healthRegenerationSlider != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "health_regeneration",
          this.healthRegenerationSlider.getX() + sliderXOffset,
          this.healthRegenerationSlider.getY() + sliderYOffset);
    }
  }
}
