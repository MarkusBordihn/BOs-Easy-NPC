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

import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class CombatAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  public CombatAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.combatAttributeButton.active = false;

    // Button rows
    int firstButtonRow = this.leftPos + 10;

    // Attribute data
    AttributeDataCapable<?> attributeData = this.getEasyNPC().getEasyNPCAttributeData();
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 25,
            CombatAttributeType.IS_INVULNERABLE.getAttributeName(),
            entityAttributes.getCombatAttributes().isInvulnerable(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_INVULNERABLE,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 45,
            CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS.getAttributeName(),
            entityAttributes.getCombatAttributes().isAttackableByPlayers(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 65,
            CombatAttributeType.IS_ATTACKABLE_BY_MONSTERS.getAttributeName(),
            entityAttributes.getCombatAttributes().isAttackableByMonsters(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_ATTACKABLE_BY_MONSTERS,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 85,
            CombatAttributeType.IS_ATTACKABLE_BY_FACTIONS.getAttributeName(),
            entityAttributes.getCombatAttributes().isAttackableByFactions(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_ATTACKABLE_BY_FACTIONS,
                        checkbox.selected())));
    this.addRenderableWidget(
        new HelpIcon(
            this.leftPos + 230, this.buttonTopPos + 87, "is_attackable_by_factions.tooltip"));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 105,
            InteractionAttributeType.CAN_BE_HIT_BY_PROJECTILE.getAttributeName(),
            entityAttributes.getInteractionAttributes().canBeHitByProjectile(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .interactionAttributeChange(
                        this.getEasyNPCUUID(),
                        InteractionAttributeType.CAN_BE_HIT_BY_PROJECTILE,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 125,
            CombatAttributeType.IS_KNOCKBACK_RESISTANT.getAttributeName(),
            entityAttributes.getCombatAttributes().isKnockbackResistant(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_KNOCKBACK_RESISTANT,
                        checkbox.selected())));

    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            this.buttonTopPos + 145,
            CombatAttributeType.IS_EXPLOSION_RESISTANT.getAttributeName(),
            entityAttributes.getCombatAttributes().isExplosionResistant(),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .combatAttributeChange(
                        this.getEasyNPCUUID(),
                        CombatAttributeType.IS_EXPLOSION_RESISTANT,
                        checkbox.selected())));
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);
  }
}
