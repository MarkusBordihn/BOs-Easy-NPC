/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.gametest.ServerMessageAssertions.SurvivalOwnerAccess;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeCombatAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeDisplayAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEntityAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEntityBaseAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeEnvironmentalAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeInteractionAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeMovementAttributeMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeNameMessage;
import de.markusbordihn.easynpc.data.attribute.CombatAttributeType;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.data.attribute.EntityAttributes;
import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextColor;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;

public final class AttributeMessageTestHelper {

  private static final ResourceLocation MAX_HEALTH =
      ResourceLocation.withDefaultNamespace("generic.max_health");
  private static final double RAISED_MAX_HEALTH = 40.0d;
  private static final int CHANGED_OPACITY = 40;
  private static final double CHANGED_HEALTH_REGENERATION = 2.5d;
  private static final double CHANGED_HOVER_HEIGHT = 1.5d;
  private static final String CHANGED_NAME = "Renamed Test NPC";
  private static final int CHANGED_NAME_COLOR = 0x55FF55;

  private AttributeMessageTestHelper() {}

  public static void assertMaxHealthChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeEntityBaseAttributeMessage(uuid, MAX_HEALTH, RAISED_MAX_HEALTH),
        ChangeEntityBaseAttributeMessage::create,
        easyNPC ->
            easyNPC.getLivingEntity().getMaxHealth() == (float) RAISED_MAX_HEALTH
                && easyNPC.getLivingEntity().getHealth() == (float) RAISED_MAX_HEALTH,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertVisibilityChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeDisplayAttributeMessage(uuid, DisplayAttributeType.VISIBLE, false),
        ChangeDisplayAttributeMessage::create,
        easyNPC ->
            !easyNPC
                .getEasyNPCDisplayAttributeData()
                .getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertOpacityChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeDisplayAttributeMessage(uuid, DisplayAttributeType.OPACITY, CHANGED_OPACITY),
        ChangeDisplayAttributeMessage::create,
        easyNPC ->
            easyNPC
                    .getEasyNPCDisplayAttributeData()
                    .getDisplayIntAttribute(DisplayAttributeType.OPACITY)
                == CHANGED_OPACITY,
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertCombatFlagChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeCombatAttributeMessage(uuid, CombatAttributeType.IS_INVULNERABLE, false),
        ChangeCombatAttributeMessage::create,
        easyNPC -> !entityAttributes(easyNPC).getCombatAttributes().isInvulnerable(),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertHealthRegenerationChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeCombatAttributeMessage(
                uuid, CombatAttributeType.HEALTH_REGENERATION, CHANGED_HEALTH_REGENERATION),
        ChangeCombatAttributeMessage::create,
        easyNPC ->
            entityAttributes(easyNPC).getCombatAttributes().healthRegeneration()
                == CHANGED_HEALTH_REGENERATION,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertMovementFlagChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeMovementAttributeMessage(uuid, MovementAttributeType.IS_IMMOVABLE, true),
        ChangeMovementAttributeMessage::create,
        easyNPC -> entityAttributes(easyNPC).getMovementAttributes().isImmovable(),
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertHoverHeightChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeMovementAttributeMessage(
                uuid, MovementAttributeType.HOVER_HEIGHT, CHANGED_HOVER_HEIGHT),
        ChangeMovementAttributeMessage::create,
        easyNPC ->
            entityAttributes(easyNPC).getMovementAttributes().hoverHeight() == CHANGED_HOVER_HEIGHT,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertNavigationTypeChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeMovementAttributeMessage(uuid, NavigationType.FLYING),
        ChangeMovementAttributeMessage::create,
        easyNPC ->
            entityAttributes(easyNPC).getMovementAttributes().navigationType()
                == NavigationType.FLYING,
        SurvivalOwnerAccess.DENIED);
  }

  public static void assertInteractionFlagChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeInteractionAttributeMessage(
                uuid, InteractionAttributeType.CAN_BE_LEASHED, true),
        ChangeInteractionAttributeMessage::create,
        easyNPC -> entityAttributes(easyNPC).getInteractionAttributes().canBeLeashed(),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertEnvironmentalFlagChange(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeEnvironmentalAttributeMessage(
                uuid, EnvironmentalAttributeType.NO_GRAVITY, true),
        ChangeEnvironmentalAttributeMessage::create,
        easyNPC ->
            entityAttributes(easyNPC).getEnvironmentalAttributes().noGravity()
                && easyNPC.getLivingEntity().isNoGravity(),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertSilentChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid -> new ChangeEntityAttributeMessage(uuid, EntityAttribute.SILENT, true),
        ChangeEntityAttributeMessage::create,
        easyNPC -> easyNPC.getEasyNPCAttributeData().getAttributeSilent(),
        SurvivalOwnerAccess.GRANTED);
  }

  public static void assertNameChange(GameTestHelper helper, EntityType<?> entityType) {
    ServerMessageAssertions.assertAppliedOnlyWithAccess(
        helper,
        entityType,
        uuid ->
            new ChangeNameMessage(
                uuid, CHANGED_NAME, CHANGED_NAME_COLOR, NameVisibilityType.MOUSE_OVER),
        ChangeNameMessage::create,
        AttributeMessageTestHelper::hasChangedName,
        SurvivalOwnerAccess.GRANTED);
  }

  private static boolean hasChangedName(EasyNPC<?> easyNPC) {
    Component customName = easyNPC.getEntity().getCustomName();
    return customName != null
        && CHANGED_NAME.equals(customName.getString())
        && TextColor.fromRgb(CHANGED_NAME_COLOR).equals(customName.getStyle().getColor())
        && easyNPC
                .getEasyNPCDisplayAttributeData()
                .getDisplayEnumAttribute(
                    DisplayAttributeType.NAME_VISIBILITY, NameVisibilityType.class)
            == NameVisibilityType.MOUSE_OVER;
  }

  private static EntityAttributes entityAttributes(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCAttributeData().getEntityAttributes();
  }
}
