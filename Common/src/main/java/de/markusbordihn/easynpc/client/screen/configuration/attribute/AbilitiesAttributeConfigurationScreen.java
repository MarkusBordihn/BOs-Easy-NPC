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

package de.markusbordihn.easynpc.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class AbilitiesAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  RangeSliderButton healthRegenerationSlider;

  public AbilitiesAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
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
    AttributeData<?> attributeData = this.getEasyNPC().getEasyNPCAttributeData();
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

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 45,
            MovementAttributeType.CAN_OPEN_DOOR.getAttributeName(),
            entityAttributes.getMovementAttributes().canOpenDoor(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .movementAttributeChange(
                        this.getEasyNPCUUID(),
                        MovementAttributeType.CAN_OPEN_DOOR,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 45,
            MovementAttributeType.CAN_CLOSE_DOOR.getAttributeName(),
            entityAttributes.getMovementAttributes().canCloseDoor(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .movementAttributeChange(
                        this.getEasyNPCUUID(),
                        MovementAttributeType.CAN_CLOSE_DOOR,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            thirdButtonRow,
            this.buttonTopPos + 45,
            MovementAttributeType.CAN_PASS_DOOR.getAttributeName(),
            entityAttributes.getMovementAttributes().canPassDoor(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .movementAttributeChange(
                        this.getEasyNPCUUID(),
                        MovementAttributeType.CAN_PASS_DOOR,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 65,
            CombatAttributeType.IS_ATTACKABLE.getAttributeName(),
            entityAttributes.getCombatAttributes().isAttackable(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_ATTACKABLE,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 85,
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
            this.buttonTopPos + 85,
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
            thirdButtonRow,
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
            firstButtonRow,
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
            this.buttonTopPos + 125,
            EntityAttribute.SILENT.getAttributeName(),
            attributeData.getAttributeSilent(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .entityAttributeChange(
                        this.getEasyNPCUUID(), EntityAttribute.SILENT, checkbox.selected())));

    this.healthRegenerationSlider =
        this.addRenderableWidget(
            new RangeSliderButton(
                firstButtonRow + 130,
                this.buttonTopPos + 150,
                CombatAttributeType.HEALTH_REGENERATION.getAttributeName(),
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
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    int sliderXOffset = -135;
    int sliderYOffset = 3;

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
