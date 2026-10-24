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

import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class NameCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private NameCommandTestHelper() {}

  private static int executeConsoleCommand(GameTestHelper helper, String command) {
    return GameTestHelpers.executeCommand(
        helper, GameTestHelpers.consoleCommandSource(helper, NPC_POSITION), command);
  }

  private static String getNameVisibility(EasyNPC<?> easyNPC) {
    return easyNPC
        .getEasyNPCDisplayAttributeData()
        .getDisplayStringAttribute(DisplayAttributeType.NAME_VISIBILITY);
  }

  public static void assertNameClearHidesName(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    int setResult =
        executeConsoleCommand(helper, "easy_npc name set " + easyNPC.getEntityUUID() + " \"Test\"");
    GameTestHelpers.assertEquals(helper, "Name set command result", 1, setResult);
    GameTestHelpers.assertEquals(
        helper,
        "Name visibility after set",
        NameVisibilityType.ALWAYS.name(),
        getNameVisibility(easyNPC));

    int clearResult =
        executeConsoleCommand(helper, "easy_npc name clear " + easyNPC.getEntityUUID());

    GameTestHelpers.assertEquals(helper, "Name clear command result", 1, clearResult);
    GameTestHelpers.assertEquals(
        helper,
        "Name visibility after clear",
        NameVisibilityType.NEVER.name(),
        getNameVisibility(easyNPC));
    GameTestHelpers.assertTrue(
        helper, "Custom name must be hidden", !easyNPC.getEntity().isCustomNameVisible());
  }
}
