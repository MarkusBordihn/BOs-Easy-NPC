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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.gametest.GameTestHelpers;
import de.markusbordihn.easynpc.network.message.NetworkMessageRecord;
import io.netty.buffer.Unpooled;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public final class ServerMessageAssertions {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);

  private ServerMessageAssertions() {}

  public static <M extends NetworkMessageRecord> void assertAppliedOnlyWithAccess(
      GameTestHelper helper,
      EntityType<?> entityType,
      Function<UUID, M> messageFactory,
      Function<FriendlyByteBuf, M> messageReader,
      Predicate<EasyNPC<?>> isApplied,
      SurvivalOwnerAccess survivalOwnerAccess) {
    assertAppliedOnlyWithAccess(
        helper,
        entityType,
        easyNPC -> {},
        messageFactory,
        messageReader,
        isApplied,
        survivalOwnerAccess);
  }

  public static <M extends NetworkMessageRecord> void assertAppliedOnlyWithAccess(
      GameTestHelper helper,
      EntityType<?> entityType,
      Consumer<EasyNPC<?>> setup,
      Function<UUID, M> messageFactory,
      Function<FriendlyByteBuf, M> messageReader,
      Predicate<EasyNPC<?>> isApplied,
      SurvivalOwnerAccess survivalOwnerAccess) {
    EasyNPC<?> strangerTarget = spawnTarget(helper, entityType, setup);
    ServerPlayer stranger =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "test-stranger");
    assertUnchangedAfter(
        helper, strangerTarget, messageFactory, messageReader, isApplied, stranger, "stranger");

    EasyNPC<?> ownerTarget = spawnTarget(helper, entityType, setup);
    ServerPlayer survivalOwner =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, "test-survival-owner");
    ownerTarget.getEasyNPCOwnerData().setNPCOwnerUUID(survivalOwner.getUUID());
    if (survivalOwnerAccess == SurvivalOwnerAccess.GRANTED) {
      assertAppliedAfter(
          helper, ownerTarget, messageFactory, messageReader, isApplied, survivalOwner);
    } else {
      assertUnchangedAfter(
          helper,
          ownerTarget,
          messageFactory,
          messageReader,
          isApplied,
          survivalOwner,
          "survival owner");
    }

    EasyNPC<?> creativeTarget = spawnTarget(helper, entityType, setup);
    ServerPlayer creativePlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "test-creative-player");
    assertAppliedAfter(
        helper, creativeTarget, messageFactory, messageReader, isApplied, creativePlayer);
  }

  private static EasyNPC<?> spawnTarget(
      GameTestHelper helper, EntityType<?> entityType, Consumer<EasyNPC<?>> setup) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    setup.accept(easyNPC);
    return easyNPC;
  }

  private static <M extends NetworkMessageRecord> void assertAppliedAfter(
      GameTestHelper helper,
      EasyNPC<?> easyNPC,
      Function<UUID, M> messageFactory,
      Function<FriendlyByteBuf, M> messageReader,
      Predicate<EasyNPC<?>> isApplied,
      ServerPlayer sender) {
    assertNotAppliedYet(helper, easyNPC, isApplied);
    receive(messageFactory, messageReader, easyNPC, sender);
    GameTestHelpers.assertTrue(
        helper,
        "Message from " + sender.getScoreboardName() + " was not applied",
        isApplied.test(easyNPC));
  }

  private static <M extends NetworkMessageRecord> void assertUnchangedAfter(
      GameTestHelper helper,
      EasyNPC<?> easyNPC,
      Function<UUID, M> messageFactory,
      Function<FriendlyByteBuf, M> messageReader,
      Predicate<EasyNPC<?>> isApplied,
      ServerPlayer sender,
      String senderRole) {
    assertNotAppliedYet(helper, easyNPC, isApplied);
    CompoundTag before = easyNPC.getEntity().saveWithoutId(new CompoundTag());
    receive(messageFactory, messageReader, easyNPC, sender);
    GameTestHelpers.assertTrue(
        helper, "Message from " + senderRole + " was applied", !isApplied.test(easyNPC));
    GameTestHelpers.assertEquals(
        helper,
        "Message from " + senderRole + " changed the NPC",
        before,
        easyNPC.getEntity().saveWithoutId(new CompoundTag()));
  }

  private static void assertNotAppliedYet(
      GameTestHelper helper, EasyNPC<?> easyNPC, Predicate<EasyNPC<?>> isApplied) {
    GameTestHelpers.assertTrue(
        helper, "Test setup already matches the expected result", !isApplied.test(easyNPC));
  }

  private static <M extends NetworkMessageRecord> void receive(
      Function<UUID, M> messageFactory,
      Function<FriendlyByteBuf, M> messageReader,
      EasyNPC<?> easyNPC,
      ServerPlayer sender) {
    FriendlyByteBuf buffer = new FriendlyByteBuf(Unpooled.buffer());
    messageFactory.apply(easyNPC.getEntityUUID()).write(buffer);
    messageReader.apply(buffer).handleServer(sender);
  }

  public enum SurvivalOwnerAccess {
    GRANTED,
    DENIED
  }
}
