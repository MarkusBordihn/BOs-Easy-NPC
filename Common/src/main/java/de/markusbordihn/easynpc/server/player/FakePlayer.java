//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package de.markusbordihn.easynpc.server.player;

import com.mojang.authlib.GameProfile;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.stats.Stat;

public class FakePlayer extends ServerPlayer {

  public FakePlayer(final ServerLevel level, final BlockPos blockPos) {
    super(level.getServer(), level, new GameProfile(UUID.randomUUID(), "FakePlayer"));
    this.getAdvancements().stopListening();
    this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
  }

  public static boolean isInvalidFakePlayer(final FakePlayer fakePlayer) {
    return !(fakePlayer instanceof FakePlayer) || !fakePlayer.isAlive();
  }

  public FakePlayer updatePosition(final ServerLevel level, final BlockPos blockPos) {
    if (this.level != level) {
      this.setLevel(level);
    } else if (this.blockPosition().distSqr(blockPos) > 1.0D) {
      this.setPos(blockPos.getX(), blockPos.getY(), blockPos.getZ());
    }
    return this;
  }

  @Override
  public void displayClientMessage(final Component chatComponent, final boolean actionBar) {
    // Suppress chat messages
  }

  @Override
  public void sendMessage(final Component component, final UUID senderUUID) {
    // Suppress chat messages
  }

  @Override
  public void awardStat(final Stat stat, final int increment) {
    // Suppress stat award
  }

  @Override
  public void tick() {
    // Suppress fake player tick
  }

  @Override
  public void doTick() {
    // Suppress fake player tick
  }
}
