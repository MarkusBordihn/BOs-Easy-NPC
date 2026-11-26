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

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.message.ServerAttributeNetworkMessageHandlerInterface;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
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

    this.combatAttributeButton.active = false;

    int firstButtonRow = this.leftPos + 10;

    AttributeDataCapable<?> attributeData = this.getEasyNPC().getEasyNPCAttributeData();
    EntityAttributes entityAttributes = attributeData.getEntityAttributes();

    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 25,
        CombatAttributeType.IS_INVULNERABLE,
        entityAttributes.getCombatAttributes().isInvulnerable(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 45,
        CombatAttributeType.IS_ATTACKABLE_BY_PLAYERS,
        entityAttributes.getCombatAttributes().isAttackableByPlayers(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 65,
        CombatAttributeType.IS_ATTACKABLE_BY_MONSTERS,
        entityAttributes.getCombatAttributes().isAttackableByMonsters(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 85,
        CombatAttributeType.IS_ATTACKABLE_BY_FACTIONS,
        entityAttributes.getCombatAttributes().isAttackableByFactions(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);
    this.addRenderableWidget(
        new HelpIcon(
            this.leftPos + 230, this.buttonTopPos + 87, "is_attackable_by_factions.tooltip"));
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 105,
        InteractionAttributeType.CAN_BE_HIT_BY_PROJECTILE,
        entityAttributes.getInteractionAttributes().canBeHitByProjectile(),
        ServerAttributeNetworkMessageHandlerInterface::interactionAttributeChange);
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 125,
        CombatAttributeType.IS_KNOCKBACK_RESISTANT,
        entityAttributes.getCombatAttributes().isKnockbackResistant(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);
    this.addAttributeCheckbox(
        firstButtonRow,
        this.buttonTopPos + 145,
        CombatAttributeType.IS_EXPLOSION_RESISTANT,
        entityAttributes.getCombatAttributes().isExplosionResistant(),
        ServerAttributeNetworkMessageHandlerInterface::combatAttributeChange);

    this.addRenderableWidget(
        new TextButton(
            firstButtonRow,
            this.buttonTopPos + 175,
            140,
            "projectile_settings",
            onPress -> this.openConfiguration(ConfigurationType.PROJECTILE_ATTRIBUTE)));
  }
}
