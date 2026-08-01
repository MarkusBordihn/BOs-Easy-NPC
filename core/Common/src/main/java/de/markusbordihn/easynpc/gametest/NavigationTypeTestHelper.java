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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.attribute.EnvironmentalAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributeType;
import de.markusbordihn.easynpc.data.attribute.MovementAttributes;
import de.markusbordihn.easynpc.data.attribute.NavigationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.control.EasyNPCFlyingMoveControl;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ai.navigation.FlyingPathNavigation;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.entity.ai.navigation.WaterBoundPathNavigation;
import net.minecraft.world.phys.Vec3;

public class NavigationTypeTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(2, 2, 2);

  private NavigationTypeTestHelper() {}

  public static void assertNavigationTypeChangesTheNavigation(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    GameTestHelpers.assertNotNull(helper, "NPC must support navigation", navigationData);

    AttributeHandler.setNavigationType(easyNPC, NavigationType.FLYING);
    GameTestHelpers.assertTrue(
        helper,
        "A flying NPC must use the flying path navigation",
        easyNPC.getMob().getNavigation() instanceof FlyingPathNavigation);
    GameTestHelpers.assertTrue(
        helper,
        "A flying NPC must use the hovering move control",
        easyNPC.getMob().getMoveControl() instanceof EasyNPCFlyingMoveControl);
    GameTestHelpers.assertTrue(
        helper, "A flying NPC must report that it can fly", navigationData.canFly());

    AttributeHandler.setNavigationType(easyNPC, NavigationType.AQUATIC);
    GameTestHelpers.assertTrue(
        helper,
        "An aquatic NPC must use the water bound path navigation",
        easyNPC.getMob().getNavigation() instanceof WaterBoundPathNavigation);
    GameTestHelpers.assertTrue(
        helper, "An aquatic NPC must not report that it can fly", !navigationData.canFly());

    AttributeHandler.setNavigationType(easyNPC, NavigationType.GROUND);
    GameTestHelpers.assertTrue(
        helper,
        "A ground NPC must use the ground path navigation again",
        easyNPC.getMob().getNavigation() instanceof GroundPathNavigation);
    GameTestHelpers.assertTrue(
        helper,
        "A ground NPC must not keep the hovering move control",
        !(easyNPC.getMob().getMoveControl() instanceof EasyNPCFlyingMoveControl));
  }

  public static void assertNavigationIsOnlyRefreshedOnChange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();

    navigationData.refreshNavigationIfChanged();
    Object appliedNavigation = easyNPC.getMob().getNavigation();
    Object appliedMoveControl = easyNPC.getMob().getMoveControl();

    navigationData.refreshNavigationIfChanged();
    GameTestHelpers.assertTrue(
        helper,
        "An unchanged navigation type must not rebuild the navigation on every tick",
        appliedNavigation == easyNPC.getMob().getNavigation());
    GameTestHelpers.assertTrue(
        helper,
        "An unchanged navigation type must not rebuild the move control on every tick",
        appliedMoveControl == easyNPC.getMob().getMoveControl());
    GameTestHelpers.assertEquals(
        helper,
        "The applied navigation type must match the configured one",
        navigationData.getNavigationType(),
        navigationData.getAppliedNavigationType());
  }

  public static void assertGravityIsRestoredAfterFlying(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    AttributeHandler.setNavigationType(easyNPC, NavigationType.FLYING);
    AttributeHandler.setNavigationType(easyNPC, NavigationType.GROUND);
    GameTestHelpers.assertTrue(
        helper,
        "An NPC without the no gravity attribute must fall again after flying",
        !easyNPC.getEntity().isNoGravity());

    AttributeHandler.setEnvironmentalAttribute(
        easyNPC, EnvironmentalAttributeType.NO_GRAVITY, true);
    AttributeHandler.setNavigationType(easyNPC, NavigationType.FLYING);
    AttributeHandler.setNavigationType(easyNPC, NavigationType.GROUND);
    GameTestHelpers.assertTrue(
        helper,
        "An NPC with the no gravity attribute must keep it after flying",
        easyNPC.getEntity().isNoGravity());
  }

  public static void assertHoverHeightStaysInRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();

    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.HOVER_HEIGHT, Double.NaN);
    GameTestHelpers.assertEquals(
        helper,
        "A hover height that is not a number must not reach the movement control",
        0.0D,
        navigationData.getHoverHeight());

    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.HOVER_HEIGHT, 4096.0D);
    GameTestHelpers.assertEquals(
        helper,
        "A hover height above the limit must be capped",
        MovementAttributes.MAX_HOVER_HEIGHT,
        navigationData.getHoverHeight());
  }

  public static void assertNavigationTypeSurvivesPresetImport(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setNavigationType(easyNPC, NavigationType.FLYING);
    AttributeHandler.setMovementAttribute(easyNPC, MovementAttributeType.HOVER_HEIGHT, 3.0D);

    EasyNPC<?> importedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(3, 2, 3));
    importedNPC
        .getEasyNPCPresetData()
        .importPresetData(easyNPC.getEasyNPCPresetData().serializePresetData());

    MovementAttributes movementAttributes =
        importedNPC.getEasyNPCAttributeData().getEntityAttributes().getMovementAttributes();
    GameTestHelpers.assertEquals(
        helper,
        "An imported preset must keep the navigation type",
        NavigationType.FLYING,
        movementAttributes.navigationType());
    GameTestHelpers.assertEquals(
        helper,
        "An imported preset must keep the hover height",
        3.0D,
        movementAttributes.hoverHeight());
    GameTestHelpers.assertTrue(
        helper,
        "An imported flying NPC must use the flying path navigation",
        importedNPC.getMob().getNavigation() instanceof FlyingPathNavigation);
  }
}
