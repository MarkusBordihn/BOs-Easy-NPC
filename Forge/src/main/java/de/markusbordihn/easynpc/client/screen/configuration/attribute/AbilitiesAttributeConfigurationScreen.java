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
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.menu.configuration.attribute.AbilitiesAttributeConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class AbilitiesAttributeConfigurationScreen
    extends AttributeConfigurationScreen<AbilitiesAttributeConfigurationMenu> {

  public AbilitiesAttributeConfigurationScreen(
      AbilitiesAttributeConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.abilitiesAttributeButton.active = false;

    int firstButtonRow = this.leftPos + 10;
    int secondButtonRow = this.leftPos + 100;
    int thirdButtonRow = this.leftPos + 200;

    // Attribute data
    AttributeData<?> attributeData = this.easyNPC.getEasyNPCAttributeData();

    // Checkboxes
    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 25,
            EntityAttribute.CAN_FLOAT.getAttributeName(),
            attributeData.getAttributeCanFloat(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_FLOAT, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 25,
            EntityAttribute.CAN_BE_LEASHED.getAttributeName(),
            attributeData.getAttributeCanBeLeashed(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_BE_LEASHED, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 45,
            EntityAttribute.CAN_OPEN_DOOR.getAttributeName(),
            attributeData.getAttributeCanOpenDoor(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_OPEN_DOOR, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 45,
            EntityAttribute.CAN_CLOSE_DOOR.getAttributeName(),
            attributeData.getAttributeCanCloseDoor(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_CLOSE_DOOR, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            thirdButtonRow,
            this.buttonTopPos + 45,
            EntityAttribute.CAN_PASS_DOOR.getAttributeName(),
            attributeData.getAttributeCanPassDoor(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_PASS_DOOR, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 65,
            EntityAttribute.IS_ATTACKABLE.getAttributeName(),
            attributeData.getAttributeIsAttackable(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.IS_ATTACKABLE, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 85,
            EntityAttribute.IS_PUSHABLE.getAttributeName(),
            attributeData.getAttributeIsPushable(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.IS_PUSHABLE, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            secondButtonRow,
            this.buttonTopPos + 85,
            EntityAttribute.PUSH_ENTITIES.getAttributeName(),
            attributeData.getAttributePushEntities(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.PUSH_ENTITIES, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 105,
            EntityAttribute.CAN_USE_NETHER_PORTAL.getAttributeName(),
            attributeData.getAttributeCanUseNetherPortal(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.CAN_USE_NETHER_PORTAL, checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 125,
            EntityAttribute.SILENT.getAttributeName(),
            attributeData.getAttributeSilent(),
            checkbox ->
                NetworkMessageHandler.entityAttributeChange(
                    uuid, EntityAttribute.SILENT, checkbox.selected())));
  }
}
